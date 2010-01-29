package erjang.beam.loader;

import java.io.IOException;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.DataInputStream;
import java.io.ByteArrayInputStream;

import erjang.EObject;
import erjang.EAtom;
import erjang.EString;
import erjang.EInputStream;
import erjang.beam.BeamOpcode;

public class BeamLoader {
    static final boolean DEBUG = true;

    public static void main(String[] args) throws IOException {
	for (String filename : args) read(filename);
    }


    public static void read(String filename) throws IOException {
	long file_size = new File(filename).length();
	DataInputStream in = null;
	try {
	    in = new DataInputStream(new FileInputStream(filename));
	    new BeamLoader(in, file_size).read();
	} finally {
	    if (in != null) in.close();
	}
    }

    //======================================================================

    // Magic numbers, file outline:
    static final int FOR1 = 0x464f5231; // "FOR1"
    static final int BEAM = 0x4245414d; // "BEAM"

    // Magic numbers, section headers:
    static final int ATOM  = 0x41746f6d; // "Atom"
    static final int CODE  = 0x436f6465; // "Code"
    static final int STR_T = 0x53747254; // "StrT"
    static final int IMP_T = 0x496d7054; // "ImpT"
    static final int EXP_T = 0x45787054; // "ExpT"
    static final int LIT_T = 0x4c697454; // "LitT"
    static final int FUN_T = 0x46756e54; // "FunT"
    static final int LOC_T = 0x4c6f6354; // "LocT"
    static final int ATTR  = 0x41747472; // "Attr"
    static final int C_INF = 0x43496e66; // "CInf"
    static final int ABST  = 0x41627374; // "Abst"

    //========== State: ==========
    private EInputStream in;
    private EAtom[] atoms;
    private EString[] strings;
    private EObject[] literals;
    private EObject attributes, compilation_info, abstract_tree;

    public BeamLoader(DataInputStream in, long actual_file_size) throws IOException {
	if (in.readInt() != FOR1) throw new IOException("Bad header. Not an IFF1 file.");
	int stated_length = in.readInt();
	if (in.readInt() != BEAM) throw new IOException("Bad header. Not a BEAM code file.");

	if (stated_length+8 != actual_file_size)
	    throw new IOException("File length is off - stated as "+(stated_length+8)+", is "+actual_file_size);

	byte[] data = new byte[stated_length-4];
	in.readFully(data);

	this.in = new EInputStream(data);
    }

    public void read() throws IOException {
	while (readSection()) { }
    }

    public boolean readSection() throws IOException {
	// Read section header:
	int tag;
	try {
// 	    if (DEBUG) System.err.println("Reading section at offset "+in.getPos());
	    tag = in.read4BE();
	    if (DEBUG) System.err.println("Reading section with tag "+Integer.toHexString(tag)+" at "+in.getPos());
	} catch (EOFException eof) {
	    return false;
	}
	int sectionLength = in.read4BE();
	int startPos = in.getPos();

	// Read section body:
	int allowed_readahead = 0;
	if (sectionLength>0) switch (tag) {
	case ATOM:  readAtomSection(); break;
	case CODE:  if (DEBUG) System.err.println("<code>"); break;
	case STR_T: readStringSection(); break;
	case IMP_T: readImportSection(); break;
	case EXP_T: readExportSection(); break;
	case FUN_T: readFunctionSection(); break;
	case LIT_T: readLiteralSection(); break;
	case LOC_T: readLocalFunctionSection(); break;
	case ATTR:  readAttributeSection(); break;
	case C_INF: readCompilationInfoSection(); break;
	case ABST:  readASTSection(); break;
	default:
	    if (DEBUG) System.err.println("Unrecognized section tag: "+Integer.toHexString(tag));
	} // switch

	int readLength = in.getPos()-startPos;
	if (readLength > sectionLength+allowed_readahead)
	    throw new IOException("Malformed section #"+Integer.toHexString(tag)+": used "+readLength+" bytes of "+sectionLength+" (pos="+in.getPos()+", a.r.="+allowed_readahead+")");
	in.setPos(startPos + ((sectionLength+3)&~3));
	return true;
    }

    public void readAtomSection() throws IOException {
	if (DEBUG) System.err.println("readAtomSection");
	int nAtoms = in.read4BE();
	if (DEBUG) System.err.println("Number of atoms: "+nAtoms);
	atoms = new EAtom[nAtoms];
	for (int i=0; i<nAtoms; i++) {
	    String atom = readString(in.read1());
	    if (DEBUG) System.err.println("- #"+(i+1)+": '"+atom+"'");
	    atoms[i] = EAtom.intern(atom);
	}
    }

    public void readStringSection() throws IOException {
	if (DEBUG) System.err.println("readStringSection");
	int nStrings = in.read4BE();
	if (DEBUG) System.err.println("Number of strings: "+nStrings);
	strings = new EString[nStrings];
	for (int i=0; i<nStrings; i++) {
	    String string = readString(in.read1());
	    if (DEBUG) System.err.println("- #"+(i+1)+": '"+string+"'");
	    strings[i] = new EString(string);
	}
    }

    public void readExportSection() throws IOException {
	if (DEBUG) System.err.println("readExportSection");
	int nExports = in.read4BE();
	if (DEBUG) System.err.println("Number of exports: "+nExports);
	for (int i=0; i<nExports; i++) {
	    int f_atm_no = in.read4BE();
	    int arity    = in.read4BE();
	    int label    = in.read4BE();
	    if (DEBUG && atoms != null) {
		System.err.println("- #"+(i+1)+": "+atoms[f_atm_no-1]+"/"+arity+" @ "+label);
	    }
	}
    }

    public void readLocalFunctionSection() throws IOException {
	if (DEBUG) System.err.println("readLocalFunctionSection");
	int nLocals = in.read4BE();
	if (DEBUG) System.err.println("Number of locals: "+nLocals);
	for (int i=0; i<nLocals; i++) {
	    int f_atm_no = in.read4BE();
	    int arity    = in.read4BE();
	    int label    = in.read4BE();
	    if (DEBUG && atoms != null) {
		System.err.println("- #"+(i+1)+": "+atoms[f_atm_no-1]+"/"+arity+" @ "+label);
	    }
	}
    }

    public void readFunctionSection() throws IOException {
	if (DEBUG) System.err.println("readFunctionSection");
	int nFunctions = in.read4BE();
	if (DEBUG) System.err.println("Number of function descrs: "+nFunctions);
	for (int i=0; i<nFunctions; i++) {
	    int f_atm_no = in.read4BE();
	    int arity    = in.read4BE();
	    int label    = in.read4BE();
	    int perm_nr  = in.read4BE();
	    int arity2    = in.read4BE();
	    int value    = in.read4BE();
	    if (DEBUG && atoms != null) {
		System.err.println("- #"+(i+1)+": "+atoms[f_atm_no-1]+"/"+arity+" @ "+label);
		System.err.println("--> "+perm_nr+"/"+arity2+" $ "+value);
	    }
	}
    }

    public void readImportSection() throws IOException {
	if (DEBUG) System.err.println("readImportSection");
	int nImports = in.read4BE();
	if (DEBUG) System.err.println("Number of exports: "+nImports);
	for (int i=0; i<nImports; i++) {
	    int m_atm_no = in.read4BE();
	    int f_atm_no = in.read4BE();
	    int arity    = in.read4BE();
	    if (DEBUG && atoms != null) {
		System.err.println("- #"+(i+1)+": "+atoms[m_atm_no-1]+":"+atoms[f_atm_no-1]+"/"+arity);
	    }
	}
    }

    public void readAttributeSection() throws IOException {
	if (DEBUG) System.err.println("readAttributeSection");
	attributes = in.read_any();
	if (DEBUG) System.err.println("Attibutes: "+attributes);
    }

    public void readCompilationInfoSection() throws IOException {
	if (DEBUG) System.err.println("readCompilationInfoSection");
	compilation_info = in.read_any();
	if (DEBUG) System.err.println("Compilation info: "+compilation_info);
    }

    public void readASTSection() throws IOException {
	if (DEBUG) System.err.println("readASTSection");
	abstract_tree = in.read_any();
// 	if (DEBUG) System.err.println("AST: "+abstract_tree);
    }

    public void readLiteralSection() throws IOException {
	if (DEBUG) System.err.println("readLiteralSection");
	final byte[] buf = in.read_size_and_inflate();
	final EInputStream is = new EInputStream(buf);
	int nLiterals = is.read4BE();
	if (DEBUG) System.err.println("Number of literals: "+nLiterals);
	literals = new EObject[nLiterals];
	for (int i=0; i<nLiterals; i++) {
	    int lit_length = is.read4BE();
	    int pos_before_lit = is.getPos();
	    literals[i] = is.read_any();
	    if (DEBUG) System.err.println("- #"+i+": "+literals[i]);
	    int pos_after_lit = is.getPos();
	    assert(pos_after_lit == pos_before_lit + lit_length);
	}
    }

    //========== Utility functions ==============================
    public String readString(int len) throws IOException {
	byte[] data = new byte[len];
	int read = in.read(data);
	assert(read==len);
	return new String(data);
    }
}
