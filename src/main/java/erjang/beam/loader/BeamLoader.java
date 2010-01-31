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

import static erjang.beam.loader.Operands.*;

public class BeamLoader extends CodeTables {
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
	try {
	if (sectionLength>0) switch (tag) {
	case ATOM:  readAtomSection(); break;
	case CODE:  readCodeSection(); break;
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
	} catch (Exception e) {
	    int relPos = in.getPos()-startPos;
	    throw new IOException("Error occurred around "+relPos+"=0x"+Integer.toHexString(relPos)+" bytes into section "+Integer.toHexString(tag), e);
	}

	int readLength = in.getPos()-startPos;
	if (readLength > sectionLength)
	    throw new IOException("Malformed section #"+Integer.toHexString(tag)+": used "+readLength+" bytes of "+sectionLength+" (pos="+in.getPos());
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
		System.err.println("- #"+(i+1)+": "+atom(f_atm_no)+"/"+arity+" @ "+label);
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
		System.err.println("- #"+(i+1)+": "+atom(f_atm_no)+"/"+arity+" @ "+label);
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
		System.err.println("- #"+(i+1)+": "+atom(f_atm_no)+"/"+arity+" @ "+label);
		System.err.println("--> "+perm_nr+"/"+arity2+" $ "+value);
	    }
	}
    }

    public void readImportSection() throws IOException {
	if (DEBUG) System.err.println("readImportSection");
	int nImports = in.read4BE();
	if (DEBUG) System.err.println("Number of imports: "+nImports);
	externalFuns = new ExtFun[nImports];
	for (int i=0; i<nImports; i++) {
	    int m_atm_no = in.read4BE();
	    int f_atm_no = in.read4BE();
	    int arity    = in.read4BE();
	    if (DEBUG && atoms != null) {
		System.err.println("- #"+(i+1)+": "+atom(m_atm_no)+":"+atom(f_atm_no)+"/"+arity);
	    externalFuns[i] = new ExtFun(atom(m_atm_no), atom(f_atm_no), arity);
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

    public void readCodeSection() throws IOException {
	if (DEBUG) System.err.println("readCodeSection");
	int dummy1 = in.read4BE();
	int zero1  = in.read4BE();
	int dummy2 = in.read4BE();
	int labelCnt = in.read4BE();
	int funCnt = in.read4BE();
	if (DEBUG) System.err.println("Code metrics: d1:"+dummy1+
				      ", z:"+zero1+
				      ", d2:"+dummy2+
				      ", L:"+labelCnt+
				      ", f:"+funCnt);

	Insn insn;
	do {
	    insn = readInstruction();
	    if (DEBUG) System.err.println(insn);
	} while (insn != null && insn.opcode() != BeamOpcode.int_code_end);
    }

    public Insn readInstruction() throws IOException {
	int opcode_no = in.read1();
	BeamOpcode opcode = BeamOpcode.decode(opcode_no);
	if (DEBUG) System.err.print("<"+opcode+">");
	if (opcode != null) {
	    switch (opcode) {
	    //---------- 0-ary ----------
	    case K_return:
	    case send:
	    case remove_message:
	    case timeout:
	    case if_end:
	    case int_code_end:
		return new Insn(opcode); // TODO: use static set of objects

	    //---------- 1-ary ----------
	    case label:
	    case deallocate:
	    case call_fun:
	    {
		int i1 = readCodeInteger();
		if (DEBUG && opcode==BeamOpcode.label) System.err.println("DB| ### label "+i1+"###");
		return new Insn.I(opcode, i1);
	    }

	    case loop_rec_end:
	    case wait:
	    case jump:
	    {
		Label lbl = readLabel();
		return new Insn.L(opcode, lbl);
	    }

	    case put:
	    case badmatch:
	    case case_end:
	    case try_case_end:
	    {
		SourceOperand src = readSource();
 		return new Insn.S(opcode, src);
	    }

	    case init:
	    case bs_context_to_binary:
	    {
		DestinationOperand dest = readDestination();
		return new Insn.D(opcode, dest);
	    }

	    case make_fun2:
	    {
		int fun_ref = readCodeInteger();
		return new Insn.F(opcode, fun_ref);
	    }

	    case try_end:
	    case catch_end:
	    case try_case:
	    {
		YReg y = readYReg();
		return new Insn.Y(opcode, y);
	    }

	    //---------- 2-ary ----------
	    case allocate:
	    case allocate_zero:
	    case test_heap:
	    case trim:
	    {
		int i1 = readCodeInteger();
		int i2 = readCodeInteger();
		return new Insn.II(opcode, i1, i2);
	    }

	    case call:
	    case call_only:
	    {
		int i1 = readCodeInteger();
		Label label = readLabel();
		return new Insn.IL(opcode, i1, label);
	    }

	    case call_ext:
	    case call_ext_only:
	    {
		int i1 = readCodeInteger();
		int ext_fun_ref = readCodeInteger();
		return new Insn.IE(opcode, i1, ext_fun_ref);
	    }

	    case move:
	    {
		SourceOperand src = readSource();
		DestinationOperand dest = readDestination();
		return new Insn.SD(opcode, src, dest);
	    }

	    case put_tuple:
	    {
		int i1 = readCodeInteger();
		DestinationOperand dest = readDestination();
 		return new Insn.ID(opcode, i1, dest);
	    }

	    case loop_rec:
	    {
		Label label = readLabel();
		DestinationOperand dest = readDestination();
		return new Insn.LD(opcode, label, dest);
	    }

	    case K_try:
	    case K_catch:
	    {
		YReg y = readYReg();
		Label label = readLabel();
		return new Insn.YL(opcode, y, label);
	    }

	    case is_integer:
	    case is_float:
	    case is_number:
	    case is_atom:
	    case is_pid:
	    case is_reference:
	    case is_port:
	    case is_nil:
	    case is_binary:
	    case is_list:
	    case is_nonempty_list:
	    case is_tuple:
	    case is_function:
	    case is_boolean:
	    case is_bitstr:
	    case wait_timeout:
	    {
		Label label = readLabel();
		SourceOperand src = readSource();
		return new Insn.LS(opcode, label, src);
	    }

	    case raise:
	    {
		SourceOperand src1 = readSource();
		SourceOperand src2 = readSource();
		return new Insn.SS(opcode, src1, src2);
	    }

	    //---------- 3-ary ----------
	    case allocate_heap:
	    case allocate_heap_zero:
	    {
		int i1 = readCodeInteger();
		int i2 = readCodeInteger();
		int i3 = readCodeInteger();
		return new Insn.III(opcode, i1, i2, i3);
	    }

	    case func_info:
	    {
		Atom mod = readAtom();
		Atom fun = readAtom();
		int arity = readCodeInteger();
		return new Insn.AAI(opcode, mod,fun,arity);
	    }

	    case call_ext_last:
	    {
		int arity = readCodeInteger();
		int fun_ref = readCodeInteger();
		int save = readCodeInteger();
		return new Insn.IEI(opcode, arity, fun_ref, save);
	    }

	    case put_list:
	    {
		SourceOperand src1 = readSource();
		SourceOperand src2 = readSource();
		DestinationOperand dest = readDestination();
		return new Insn.SSD(opcode, src1, src2, dest);
	    }

	    case get_tuple_element:
	    {
		SourceOperand src = readSource();
		int i = readCodeInteger();
		DestinationOperand dest = readDestination();
		return new Insn.SID(opcode, src, i, dest);
	    }

	    case set_tuple_element:
	    {
		SourceOperand src = readSource();
		DestinationOperand dest = readDestination();
		int i = readCodeInteger();
		return new Insn.SDI(opcode, src, dest, i);
	    }

	    case get_list:
	    {
		SourceOperand src = readSource();
		DestinationOperand dest1 = readDestination();
		DestinationOperand dest2 = readDestination();
		return new Insn.SDD(opcode, src, dest1, dest2);
	    }

	    case test_arity:
	    {
		Label label = readLabel();
		SourceOperand src = readSource();
		int i1 = readCodeInteger();
		return new Insn.LSI(opcode, label, src, i1);
	    }

	    case is_lt:
	    case is_ge:
	    case is_eq:
	    case is_ne:
	    case is_eq_exact:
	    case is_ne_exact:
	    {
		Label label = readLabel();
		SourceOperand src1 = readSource();
		SourceOperand src2 = readSource();
		return new Insn.LSS(opcode, label, src1, src2);
	    }

	    case call_last:
	    {
		int i1 = readCodeInteger();
		Label label = readLabel();
		int i3 = readCodeInteger();
		return new Insn.ILI(opcode, i1, label, i3);
	    }

	    case bs_start_match2:
	    {
		Label label = readLabel();
		SourceOperand src = readSource();
		int i3 = readCodeInteger();
		int i4 = readCodeInteger();
		DestinationOperand dest = readDestination();
		return new Insn.LSIID(opcode, label, src, i3, i4, dest);
	    }

	    case select_val:
	    case select_tuple_arity:
	    {
		SourceOperand src = readSource();
		Label defaultLbl = readLabel();
		Operands.List jumpTable = readList();
		return new Insn.Select(opcode, src, defaultLbl, jumpTable);
	    }

	    //---------- BIFs ----------
	    case bif0: {
		Label optLabel = readOptionalLabel();
		int ext_fun_ref = in.read1();
		DestinationOperand dest = readDestination();
		return new Insn.LED(opcode, optLabel, ext_fun_ref, dest);
	    }
	    case bif1: {
		Label optLabel = readOptionalLabel();
		int ext_fun_ref = in.read1();
		SourceOperand arg = readSource();
		DestinationOperand dest = readDestination();
		return new Insn.LESD(opcode, optLabel, ext_fun_ref, arg, dest);
	    }
	    case bif2: {
		Label optLabel = readOptionalLabel();
		int save = in.read1();
		int ext_fun_ref = in.read1();
		SourceOperand arg1 = readSource();
		SourceOperand arg2 = readSource();
		DestinationOperand dest = readDestination();
		return new Insn.LEISSD(opcode, optLabel, ext_fun_ref, save, arg1, arg2, dest);
	    }

	    case gc_bif1: {
		Label optLabel = readOptionalLabel();
		int save = readCodeInteger();
		int ext_fun_ref = readCodeInteger();
		SourceOperand arg = readSource();
		DestinationOperand dest = readDestination();
		return new Insn.LEISD(opcode, optLabel, ext_fun_ref, save, arg, dest);
	    }
	    case gc_bif2: {
		Label optLabel = readOptionalLabel();
		int save = readCodeInteger();
		int ext_fun_ref = readCodeInteger();
		SourceOperand arg1 = readSource();
		SourceOperand arg2 = readSource();
		DestinationOperand dest = readDestination();
		return new Insn.LEISSD(opcode, optLabel, ext_fun_ref, save, arg1, arg2, dest);
	    }

	    default:
		throw new IOException("Unknown instruction: "+opcode);
	    } // switch
	} else System.err.println("***unknown: 0x"+Integer.toHexString(opcode_no)+"***");
	return null;
    }

    //========== Utility functions ==============================
    public String readString(int len) throws IOException {
	byte[] data = new byte[len];
	int read = in.read(data);
	assert(read==len);
	return new String(data);
    }

    //========== Operand tags:
    //TODO: These need to be cleared up. Look at a bit less.
    static final int INT4_TAG = 0;
    static final int INTLIT4_TAG = 1;
    static final int ATOM4_TAG = 2;
    static final int XREG4_TAG = 3;
    static final int YREG4_TAG = 4;
    static final int LABEL4_TAG = 5;
    static final int EXTENDED_TAG = 7;

    static final int INT12_TAG = 8;
    static final int BIGINT_TAG = 9;
    static final int ATOM12_TAG = 10;
    static final int XREG12_TAG = 11;
    static final int YREG12_TAG = 12;
    static final int LABEL12_TAG = 13;
    static final int EXTENDED2_TAG = 7;

    //========== Extended operand tags:
    static final int LIST_TAG2 = 1;
    static final int LITERAL_TAG2 = 4;

    int peekTag() throws IOException {
	return in.peek() & 0x0F;
    }

    public SourceOperand readSource() throws IOException {
	return readOperand().asSource();
    }
    public DestinationOperand readDestination() throws IOException {
	return readOperand().asDestination();
    }

    public Label readOptionalLabel() throws IOException {
	return (peekTag() == LABEL4_TAG || peekTag() == LABEL12_TAG)
	    ? readLabel()
	    : null; // 'nofail'
    }

    public Label readLabel() throws IOException {
	return readOperand().asLabel();
    }

    public Atom readAtom() throws IOException {
	return readOperand().asAtom();
    }

    public Operands.List readList() throws IOException {
	return readOperand().asList();
    }

    public YReg readYReg() throws IOException {
	return readOperand().asYReg();
    }

    public int readCodeInteger() throws IOException {
	int d1 = in.read1();
	int tag = d1 & 0x07;
	if ((d1 & 0x07) == INT4_TAG)
	    return readSmallIntValue(d1);
	else
	    throw new IOException("Not a smallint: "+readOperand(d1));
    }

    public Operand readOperand() throws IOException {
	int d1 = in.read1();
	return readOperand(d1);
    }
    public Operand readOperand(int d1) throws IOException {
	int tag = d1 & 0x0F;
	switch (tag) {
	case INTLIT4_TAG:
	    return new Operands.Int(readSmallIntValue(d1));

  	case BIGINT_TAG: {
	    int hdata  = d1>>4;
	    if ((hdata & 1) == 0) { // Fixed-length
		return new Operands.Int((hdata << 7) + in.read1());
	    } else {
		int len = 2+(hdata>>1);
		byte d[] = new byte[len];
		in.readFully(d);
		return Operands.makeInt(d);
	    }
	}

	case ATOM4_TAG:
	case ATOM12_TAG:
	{
	    int nr = readSmallIntValue(d1);
	    return (nr==0)? Operands.Nil : new Operands.Atom(atom(nr));
	}

	case XREG4_TAG:
	case XREG12_TAG:
	{
 	    int nr = readSmallIntValue(d1);
	    return XReg.get(nr);
	}
	case YREG4_TAG:
	case YREG12_TAG:
	{
 	    int nr = readSmallIntValue(d1);
	    return YReg.get(nr);
	}
	case LABEL4_TAG:
	case LABEL12_TAG:
	{
	    int nr = readSmallIntValue(d1);
	    return new Label(nr);
	}
	case EXTENDED_TAG:
	{
	    int moretag = d1>>4;
	    switch (moretag) {
	    case LIST_TAG2: {
		int length = readSmallIntValue(in.read1());
		Operand[] list = new Operand[length];
		for (int i=0; i<length; i++) {
		    list[i] = readOperand();
		}
		return new Operands.List(list);
	    }
	    case LITERAL_TAG2: {
		int nr = readSmallIntValue(in.read1());
		return new TableLiteral(nr);
	    }
	    default:
		System.err.println("*** Unhandled extended operand tag: "+tag);
	    } // switch
	    break;
	}
	default:
	    System.err.println("*** Unhandled operand tag: "+tag);
	} // switch
	return null;
    }


    public int readSmallIntValue(int head) throws IOException {
	int tag = head & 0x0F;
	int hdata  = head>>4;
	if ((tag & 0x08) == 0) {
	    return hdata;
	} else {
	    return (hdata<<7) + in.read1();
	}
    }
}
