/** -*- tab-width: 4 -*-
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package erjang.beam.loader;

import static erjang.beam.CodeAtoms.BEAM_FILE_ATOM;
import static erjang.beam.CodeAtoms.FUNCTION_ATOM;
import static erjang.beam.CodeAtoms.START_ATOM;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;

import erjang.EAtom;
import erjang.EBinary;
import erjang.EInputStream;
import erjang.EObject;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.beam.BeamOpcode;

import erjang.beam.repr.CodeTables;
import erjang.beam.repr.ModuleRepr;
import erjang.beam.repr.FunctionRepr;
import erjang.beam.repr.Insn;
import erjang.beam.repr.Operands;
import erjang.beam.repr.Operands.AllocList;
import erjang.beam.repr.Operands.Atom;
import erjang.beam.repr.Operands.BitString;
import erjang.beam.repr.Operands.ByteString;
import erjang.beam.repr.Operands.DestinationOperand;
import erjang.beam.repr.Operands.FReg;
import erjang.beam.repr.Operands.Label;
import erjang.beam.repr.Operands.Literal;
import erjang.beam.repr.Operands.Operand;
import erjang.beam.repr.Operands.SelectList;
import erjang.beam.repr.Operands.SourceOperand;
import erjang.beam.repr.Operands.TableLiteral;
import erjang.beam.repr.Operands.XReg;
import erjang.beam.repr.Operands.YReg;
import erjang.beam.repr.ExtFun;
import erjang.beam.repr.AnonFun;
import erjang.beam.repr.FunctionInfo;

public class BeamLoader extends CodeTables {
    static final boolean DEBUG = false;
    static final boolean DEBUG_ON_ERROR = true;

	/** For testing purposes. */
    public static void main(String[] args) throws IOException {
		for (String filename : args) read(filename);
    }

    public static ModuleRepr read(String filename) throws IOException {
		long file_size = new File(filename).length();
		DataInputStream in = null;
		try {
			in = new DataInputStream(new FileInputStream(filename));
			BeamLoader bl = new BeamLoader(in, file_size, false);
			bl.read();
			return bl.toModuleRepr();
		} finally {
			if (in != null) in.close();
		}
    }

    public static ModuleRepr parse(byte[] data) throws IOException {
		ByteArrayInputStream in = new ByteArrayInputStream(data);
		BeamLoader bl = new BeamLoader(new DataInputStream(in), data.length, false);
		bl.read();
		return bl.toModuleRepr();
    }

    //======================================================================
    private EInputStream in;
	private boolean include_debug_info;
    private EObject attributes, compilation_info, abstract_tree;
    private FunctionInfo[] exports = new FunctionInfo[0], localFunctions = new FunctionInfo[0];
    private ArrayList<Insn> code;
	private ArrayList<FunctionRepr> functionReprs;
	private EBinary module_md5;
    //======================================================================
	public ModuleRepr toModuleRepr() {
		FunctionRepr[] functions = new FunctionRepr[functionReprs.size()];
		functions = functionReprs.toArray(functions);
		return new ModuleRepr(this,
							  atom(1), exports,
							  (ESeq)attributes, (ESeq)compilation_info,
							  functions);
	}

    //======================================================================

    // Magic numbers, file outline:
    static final int FOR1 = 0x464f5231; // "FOR1"
    static final int BEAM = 0x4245414d; // "BEAM"

	static final int GZIP = 0x1f8b0000; // GZIP header

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

	static final int[] SECTION_READING_ORDER = {
		ATOM, STR_T, LIT_T,		// Have no dependencies.
		IMP_T, EXP_T, FUN_T, LOC_T,	// Function tables - depend on atom table.
		CODE,					// Depends on virtually all tables.
		C_INF, ATTR, ABST		// Less vital sections.
	};

    // TODO: Take an InputStream instead of a DataInputStream (to avoid overhead when we start out with a ByteArrayInputStream).
    public BeamLoader(DataInputStream in, long actual_file_size, boolean include_debug_info) throws IOException {
		this.include_debug_info = include_debug_info;
		int header;

		in = new DataInputStream(new java.io.BufferedInputStream(in));

		boolean zipped = false;

		in.mark(8);
		if ((header=in.readInt()) != FOR1) {

			if ((header & 0xffff0000) == GZIP) {
				in.reset();

				in = new DataInputStream(new java.util.zip.GZIPInputStream(in));
				zipped = true;
				

				if (in.readInt() != FOR1) {
					throw new IOException("Bad header. Not an IFF1 file.");
				}

			} else {
				throw new IOException("Bad header. Not an IFF1 file; header: "+ Integer.toHexString(header));
			}

		}
		int stated_length = in.readInt();
		if (in.readInt() != BEAM) throw new IOException("Bad header. Not a BEAM code file.");

		if (!zipped && (stated_length+8 != actual_file_size))
			throw new IOException("File length is off - stated as "+(stated_length+8)+", is "+actual_file_size);

		byte[] data = new byte[stated_length-4];
		in.readFully(data);

		this.in = new EInputStream(data);
    }

    public void read() throws IOException {
		/* We want to process sections in an order which avoids forward
		 * references.  (For instance, many sections refer to the atoms table,
		 * so we want to process the atoms table early on.)
		 * For this reason, we first create a section directory, then
		 * process the sections in a suitable order.
		 */
		final HashMap<Integer,SectionMetadata> section_map =
			new HashMap<Integer,SectionMetadata>();

		for (SectionMetadata smd; (smd = readSectionHeader()) != null; ) {
			section_map.put(smd.tag, smd);

			// Skip to next section header:
			in.setPos(smd.offset + ((smd.length+3)&~3));
		}
		
		compute_module_md5(section_map);
		
		for (int tag : SECTION_READING_ORDER) {
			SectionMetadata smd = section_map.get(tag);
			if (smd != null) readSection(smd);
		}
		functionReprs = partitionCodeByFunction();
    }

	static final int[] SECTION_MD5_ORDER = {
		ATOM,
		CODE,
		STR_T,
		IMP_T,
		EXP_T,
	};

	private void compute_module_md5(final HashMap<Integer, SectionMetadata> section_map)
			 {
		SectionMetadata smd;
		MessageDigest context;
		try {
			context = MessageDigest.getInstance("MD5");
		} catch (NoSuchAlgorithmException e) {
			return;
		}
		
		for (int tag : SECTION_MD5_ORDER) {
			smd = section_map.get(tag);
			in.updateMessageDigest(context, smd.offset, smd.length);
		}
		
		if ((smd = section_map.get(FUN_T)) != null) {
			int start = smd.offset;
			int left = smd.length;
			
			if (left >= 4) {
				byte[] zero = new byte[4];
				in.updateMessageDigest(context, start, 4);
				start += 4;
				left -= 4;

				while (left >= 24) {
					in.updateMessageDigest(context, start, 20);
					context.update(zero, 0, 4);
					start += 24;
					left -= 24;
				}
			}

			if (left > 0) {
				in.updateMessageDigest(context, start, left);
			}
		}
		
		if ((smd = section_map.get(LIT_T)) != null) {
			in.updateMessageDigest(context, smd.offset, smd.length);
		}
		
		byte[] digest = context.digest();
		this.module_md5 = new EBinary(digest);
	}

	public ArrayList<FunctionRepr> partitionCodeByFunction() {
		int funCount = (exports==null?0:exports.length) 
			+ (localFunctions==null?0:localFunctions.length);
		ArrayList<FunctionRepr> functions = new ArrayList<FunctionRepr>(funCount);

		FunctionInfo fi = null;
		ArrayList<Insn> currentFunctionBody = null;
		for (Insn insn : code) {
			FunctionInfo newFI = null;
			if (insn.opcode() == BeamOpcode.label) { // We might switch to a new function
				int labelNr = ((Insn.I)insn).i1;
				newFI = functionAtLabel(labelNr+1);
				if (newFI==null) newFI = functionAtLabel(labelNr);
			} else if (insn.opcode() == BeamOpcode.int_code_end) {
				newFI = new FunctionInfo(null,null,-1,-1); // Easy way to handle last function
			}
			if (newFI != null && newFI != fi) { // Do switch
				if (fi != null) { // Add previous
					FunctionRepr fun = new FunctionRepr(fi, currentFunctionBody);
					functions.add(fun);
				}
				fi = newFI;
				currentFunctionBody = new ArrayList<Insn>();
			}
			// currentFunctionBody and fi are now updated.
			currentFunctionBody.add(insn);
		}

		return functions;
	}

    public SectionMetadata readSectionHeader() throws IOException {
		int tag;
		try {
			tag = in.read4BE();
			if (DEBUG) System.err.println("Reading section with tag "+toSymbolicTag(tag)+" at "+in.getPos());
		} catch (EOFException eof) {
			return null;
		}
		int sectionLength = in.read4BE();
		int startPos = in.getPos();

		return new SectionMetadata(tag, startPos, sectionLength);
	}

    public void readSection(SectionMetadata section) throws IOException {
		in.setPos(section.offset);

		// Read section body:
		try {
			if (section.length>0) switch (section.tag) {
			case ATOM:  readAtomSection(); break;
			case CODE:  readCodeSection(); break;
			case STR_T: readStringSection(section.length); break;
			case IMP_T: readImportSection(); break;
			case EXP_T: readExportSection(); break;
			case FUN_T: readFunctionSection(); break;
			case LIT_T: readLiteralSection(); break;
			case LOC_T: readLocalFunctionSection(); break;
			case ATTR:  readAttributeSection(); break;
			case C_INF: readCompilationInfoSection(); break;
			case ABST:  readASTSection(); break;
			default:
				if (DEBUG) System.err.println("Unrecognized section tag: "+Integer.toHexString(section.tag));
			} // switch
		} catch (Exception e) {
			int relPos = in.getPos()-section.offset;
			try {
				int curPos = in.getPos();
				in.setPos(curPos-16);
				byte[] d = new byte[64];
				int ctxlen = in.read(d);
				if (DEBUG_ON_ERROR) {
					System.err.println("Context dump: ");
					for (int i=0; i<ctxlen; i++) {
						int byt = d[i] & 0xFF;
						if (byt<16) System.err.print("0");
						System.err.print(Integer.toHexString(byt & 0xFF));
						System.err.print(" ");
						if ((i+1) % 16 == 0 || (i+1)==ctxlen) System.err.println();
					}
				}
			} catch (Exception e2) {}
			throw new IOException("Error occurred around "+relPos+"=0x"+Integer.toHexString(relPos)+" bytes into section "+Integer.toHexString(section.tag), e);
		}

		int readLength = in.getPos()-section.offset;
		if (readLength > section.length)
			throw new IOException("Malformed section #"+Integer.toHexString(section.tag)+": used "+readLength+" bytes of "+section.length+" (pos="+in.getPos());
// 		in.setPos(startPos + ((sectionLength+3)&~3));
    }

    /**
	 * @param tag
	 * @return
	 */
	private String toSymbolicTag(int tag) {
		char[] sym = new char[4];

		for (int i = 3, d = tag;  i >= 0;  i--, d >>>= 8) {
			sym[i] = (char) (d & 0xff);
			if (!Character.isJavaIdentifierPart(sym[i])) {
				return "0x" + Integer.toHexString(tag);
			}
		}

		return new String(sym);
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

    public void readStringSection(int sectionLength) throws IOException {
		if (DEBUG) System.err.println("readStringSection");
		stringpool = readBinary(sectionLength);
    }

    public void readExportSection() throws IOException {
		if (DEBUG) System.err.println("readExportSection");
		int nExports = in.read4BE();
		exports = new FunctionInfo[nExports];
		if (DEBUG) System.err.println("Number of exports: "+nExports);
		EAtom mod = moduleName();
		for (int i=0; i<nExports; i++) {
			int fun_atom_nr = in.read4BE();
			int arity    = in.read4BE();
			int label    = in.read4BE();
			EAtom fun = atom(fun_atom_nr);
			exports[i] = new FunctionInfo(mod, fun, arity, label);
			addFunctionAtLabel(exports[i]);
			if (DEBUG && atoms != null) {
				System.err.println("- #"+(i+1)+": "+atom(fun_atom_nr)+"/"+arity+" @ "+label);
			}
		}
    }

    public void readLocalFunctionSection() throws IOException {
		if (DEBUG) System.err.println("readLocalFunctionSection");
		int nLocals = in.read4BE();
		localFunctions = new FunctionInfo[nLocals];
		if (DEBUG) System.err.println("Number of locals: "+nLocals);
		EAtom mod = moduleName();
		for (int i=0; i<nLocals; i++) {
			int fun_atom_nr = in.read4BE();
			int arity    = in.read4BE();
			int label    = in.read4BE();
			EAtom fun = atom(fun_atom_nr);
			localFunctions[i] = new FunctionInfo(mod, fun, arity, label);
			addFunctionAtLabel(localFunctions[i]);
			if (DEBUG && atoms != null) {
				System.err.println("- #"+(i+1)+": "+atom(fun_atom_nr)+"/"+arity+" @ "+label);
			}
		}
    }

    public void readFunctionSection() throws IOException {
		if (DEBUG) System.err.println("readFunctionSection");
		int nFunctions = in.read4BE();
		anonymousFuns = new AnonFun[nFunctions];
		if (DEBUG) System.err.println("Number of function descrs: "+nFunctions);
		EAtom mod = moduleName();
		for (int i=0; i<nFunctions; i++) {
			int fun_atom_nr = in.read4BE();
			int arity     = in.read4BE();
			int label     = in.read4BE();
			int index     = in.read4BE();
			int free_vars = in.read4BE();
			int old_uniq  = in.read4BE();
			EAtom fun = atom(fun_atom_nr);
			anonymousFuns[i] = new AnonFun(mod, fun, arity, label,
					old_uniq, i, module_md5, index, free_vars);

			if (DEBUG && atoms != null) {
				System.err.println("- #"+(i+1)+": "+fun+"/"+arity+" @ "+label);
				System.err.println("--> occur:"+index+" free:"+free_vars+" $ "+old_uniq);
			}
		}
    }

	/** readImportSection
	 *  Depends on atom table. */
    public void readImportSection() throws IOException {
		if (DEBUG) System.err.println("readImportSection");
		int nImports = in.read4BE();
		if (DEBUG) System.err.println("Number of imports: "+nImports);
		externalFuns = new ExtFun[nImports];
		for (int i=0; i<nImports; i++) {
			int m_atm_no = in.read4BE();
			int f_atm_no = in.read4BE();
			int arity    = in.read4BE();
			EAtom mod = atom(m_atm_no), fun = atom(f_atm_no);
			externalFuns[i] = new ExtFun(mod, fun, arity);
			if (DEBUG && atoms != null) {
				System.err.println("- #"+(i+1)+": "+mod+":"+fun+"/"+arity);
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
		if (!include_debug_info) return;
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
		int flags = in.read4BE(); // Only 16 ever seen
		int zero  = in.read4BE(); // Only 0 ever seen
		int highestOpcode = in.read4BE();
		int labelCnt = in.read4BE();
		int funCnt = in.read4BE();
		if (DEBUG) System.err.println("Code metrics: flags:"+flags+
									  ", z:"+zero+
									  ", hop:"+highestOpcode+
									  ", L:"+labelCnt+
									  ", f:"+funCnt);

		code = new ArrayList<Insn>();
		Insn insn;
		do {
			insn = readInstruction();
			code.add(insn);
		} while (insn.opcode() != BeamOpcode.int_code_end);
    }

    public Insn readInstruction() throws IOException {
		int opcode_no = in.read1();
		BeamOpcode opcode = BeamOpcode.decode(opcode_no);
		if (opcode != null) {
			switch (opcode) {
				//---------- 0-ary ----------
			case K_return:
			case send:
			case remove_message:
			case timeout:
			case if_end:
			case int_code_end:
			case fclearerror:
			case bs_init_writable:
			case on_load:
				return new Insn(opcode); // TODO: use static set of objects

				//---------- 1-ary ----------
			case label:
			case deallocate:
			case call_fun:
			case apply:
			{
				int i1 = readCodeInteger();
				if (DEBUG && opcode==BeamOpcode.label) System.err.println("DB| ### label "+i1+"###");
				return new Insn.I(opcode, i1);
			}

			case loop_rec_end:
			case wait:
			case jump:
			case fcheckerror:
			case recv_mark:
			case recv_set:
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
				return new Insn.F(opcode, fun_ref, anonFun(fun_ref));
			}

			case try_end:
			case catch_end:
			case try_case:
			{
				YReg y = readYReg();
				return new Insn.Y(opcode, y);
			}

			case bs_put_string:
			{
				ByteString bin = readBytestringRef();
				return new Insn.By(opcode, bin);
			}

			//---------- 2-ary ----------
			case allocate:
			case allocate_zero:
			case trim:
			case apply_last:
			{
				int i1 = readCodeInteger();
				int i2 = readCodeInteger();
				return new Insn.II(opcode, i1, i2);
			}

			case test_heap:
			{
				AllocList al = readAllocList();
				int i2 = readCodeInteger();
				return new Insn.WI(opcode, al, i2);
			}

			case call:
			case call_only:
			{
				int i1 = readCodeInteger();
				Label label = readLabel();
				return new Insn.IL(opcode, i1, label,
								   functionAtLabel(label.nr));
			}

			case call_ext:
			case call_ext_only:
			{
				int i1 = readCodeInteger();
				int ext_fun_ref = readCodeInteger();
				return new Insn.IE(opcode, i1, extFun(ext_fun_ref));
			}

			case bs_save2:
			case bs_restore2:
			{
				DestinationOperand dest = readDestination();

				int i2;
				if ((peekTag() & 0x7) == ATOM4_TAG) {
					if (readAtom().getEAtom() != START_ATOM)
						throw new IOException("integer or 'start' expected");
					i2 = -1;
				} else i2 = readCodeInteger();
				return new Insn.DI(opcode, dest, i2, true);
			}

			case move:
			case fmove:
			case fconv:
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
			{
				Label label = readLabel();
				DestinationOperand src = readDestination();
				return new Insn.LD(opcode, label, src, true);
			}

			case wait_timeout:
			{
				Label label = readLabel();
				SourceOperand src = readSource();
				return new Insn.LS(opcode, label, src, false);
			}

			case raise:
			{
				SourceOperand src1 = readSource();
				SourceOperand src2 = readSource();
				return new Insn.SS(opcode, src1, src2);
			}

			case put_string:
			{
				ByteString bin = readBytestringRef();
				DestinationOperand dest = readDestination();
				return new Insn.ByD(opcode, bin, dest);
			}

			//---------- 3-ary ----------
			case allocate_heap:
			case allocate_heap_zero:
			{
				int i1 = readCodeInteger();
				AllocList al = readAllocList();
				int i3 = readCodeInteger();
				return new Insn.IWI(opcode, i1, al, i3);
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
				int ext_fun_ref = readCodeInteger();
				int dealloc = readCodeInteger();
				return new Insn.IEI(opcode, arity, extFun(ext_fun_ref), dealloc);
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
			case bs_test_tail2:
			case bs_test_unit:
			{
				Label label = readLabel();
				DestinationOperand dest = readDestination();
				int i1 = readCodeInteger();
				return new Insn.LDI(opcode, label, dest, i1, true);
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
				return new Insn.LSS(opcode, label, src1, src2, true);
			}

			case is_function2:
			{
				Label label = readLabel();
				DestinationOperand dest = readDestination();
				SourceOperand src = readSource();
				return new Insn.LDS(opcode, label, dest, src, true);
			}

			case fnegate:
			case bs_utf8_size:
			case bs_utf16_size:
			{
				Label label = readLabel();
				SourceOperand src = readSource();
				DestinationOperand dest = readDestination();
				return new Insn.LSD(opcode, label, src, dest);
			}

			case call_last:
			{
				int i1 = readCodeInteger();
				Label label = readLabel();
				int i3 = readCodeInteger();
				return new Insn.ILI(opcode, i1, label, i3,
									functionAtLabel(label.nr));
			}

			case fadd:
			case fsub:
			case fmul:
			case fdiv:
			{
				Label label = readLabel();
				SourceOperand src1 = readSource();
				SourceOperand src2 = readSource();
				DestinationOperand dest = readDestination();
				return new Insn.LSSD(opcode, label, src1, src2, dest, true);
			}

			case bs_add:
			{
				Label label = readLabel();
				SourceOperand src1 = readSource();
				SourceOperand src2 = readSource();
				int i3 = readCodeInteger();
				DestinationOperand dest = readDestination();
				return new Insn.LSSID(opcode, label, src1, src2, i3, dest);
			}

			case bs_skip_utf8:
			case bs_skip_utf16:
			case bs_skip_utf32:
			{
				Label label = readLabel();
				DestinationOperand dest = readDestination();
				int i3 = readCodeInteger();
				int i4 = readCodeInteger();
				return new Insn.LDII(opcode, label, dest, i3, i4);
			}

			case bs_match_string:
			{
				Label label = readLabel();
				DestinationOperand dest = readDestination();
				BitString bin = readBitstringRef();
				return new Insn.LDBi(opcode, label, dest, bin);
			}

			case bs_put_utf8:
			case bs_put_utf16:
			case bs_put_utf32:
			{
				Label label = readLabel();
				int i2 = readCodeInteger();
				SourceOperand src = readSource();
				return new Insn.LIS(opcode, label, i2, src, true);
			}

			case bs_start_match2:
			case bs_get_utf8:
			case bs_get_utf16:
			case bs_get_utf32:
			{
				Label label = readLabel();
				DestinationOperand dest1 = readDestination();
				int i3 = readCodeInteger();
				int i4 = readCodeInteger();
				DestinationOperand dest2 = readDestination();
				return new Insn.LDIID(opcode, label, dest1, i3, i4, dest2,
									  opcode != BeamOpcode.bs_start_match2);
			}

			case bs_put_integer:
			case bs_put_float:
			case bs_put_binary:
			{
				Label label = readLabel();
				SourceOperand src2 = readSource();
				int i3 = readCodeInteger();
				int i4 = readCodeInteger();
				SourceOperand src5 = readSource();
				return new Insn.LSIIS(opcode, label, src2, i3, i4, src5, true);
			}

			case bs_init2:
			case bs_init_bits:
			{
				Label label = readOptionalLabel();
				SourceOperand src2;
				if ((peekTag() & 0x7) == CODEINT4_TAG) {
					int i2 = readCodeInteger();
					src2 = new Operands.Int(i2);
				} else {
					src2 = readSource();
				}
				int i3 = readCodeInteger();
				int i4 = readCodeInteger();
				int i5 = readCodeInteger();
				DestinationOperand dest = readDestination();
				return new Insn.LSIIID(opcode, label, src2, i3, i4, i5, dest, true);
			}

			case bs_skip_bits2:
			{
				Label label = readLabel();
				DestinationOperand dest = readDestination();
				SourceOperand src = readSource();
				int i3 = readCodeInteger();
				int i4 = readCodeInteger();
				return new Insn.LDSII(opcode, label, dest, src, i3, i4);
			}

			case bs_get_integer2:
			case bs_get_float2:
			case bs_get_binary2:
			{
				Label label = readLabel();
				DestinationOperand dest2 = readDestination();
				int i3 = readCodeInteger();
				SourceOperand src4 = readSource();
				int i5 = readCodeInteger();
				int i6 = readCodeInteger();
				DestinationOperand dest = readDestination();
				return new Insn.LDISIID(opcode, label, dest2, i3, src4, i5, i6, dest);
			}

			case bs_append: // LSIIISIS
			{
				Label label = readLabel();
				SourceOperand src2 = readSource();
				int i3 = readCodeInteger();
				int i4 = readCodeInteger();
				int i5 = readCodeInteger();
				SourceOperand src6 = readSource();
				int i7 = readCodeInteger();
				DestinationOperand dest8 = readDestination();
				return new Insn.BSAppend(opcode, label, src2, i3, i4, i5, src6, i7, dest8);
			}

			case bs_private_append: // LSISID
			{
				Label label = readLabel();
				SourceOperand src2 = readSource();
				int i3 = readCodeInteger();
				SourceOperand src4 = readSource();
				int i5 = readCodeInteger();
				DestinationOperand dest = readDestination();
				return new Insn.BSPrivateAppend(opcode, label, src2, i3, src4, i5, dest);
			}

			case select_val:
			case select_tuple_arity:
			{
				SourceOperand src = readSource();
				Label defaultLbl = readLabel();
				SelectList jumpTable = readSelectList();
				return new Insn.Select(opcode, src, defaultLbl, jumpTable);
			}

			//---------- BIFs ----------
			case bif0: {
				Label optLabel = readOptionalLabel();
				int ext_fun_ref = readCodeInteger();
				DestinationOperand dest = readDestination();
				return new Insn.Bif(opcode, optLabel, extFun(ext_fun_ref), dest);
			}
			case bif1: {
				Label optLabel = readOptionalLabel();
				int ext_fun_ref = readCodeInteger();
				SourceOperand arg = readSource();
				DestinationOperand dest = readDestination();
				return new Insn.Bif(opcode, optLabel, extFun(ext_fun_ref), arg, dest);
			}
			case bif2: {
				Label optLabel = readOptionalLabel();
				int ext_fun_ref = readCodeInteger();
				SourceOperand arg1 = readSource();
				SourceOperand arg2 = readSource();
				DestinationOperand dest = readDestination();
				return new Insn.Bif(opcode, optLabel, extFun(ext_fun_ref), arg1, arg2, dest);
			}

			case gc_bif1: {
				Label optLabel = readOptionalLabel();
				int save = readCodeInteger();
				int ext_fun_ref = readCodeInteger();
				SourceOperand arg = readSource();
				DestinationOperand dest = readDestination();
				return new Insn.GcBif(opcode, optLabel, extFun(ext_fun_ref), save, arg, dest);
			}
			case gc_bif2: {
				Label optLabel = readOptionalLabel();
				int save = readCodeInteger();
				int ext_fun_ref = readCodeInteger();
				SourceOperand arg1 = readSource();
				SourceOperand arg2 = readSource();
				DestinationOperand dest = readDestination();
				return new Insn.GcBif(opcode, optLabel, extFun(ext_fun_ref), save, arg1, arg2, dest);
			}

			default:
				throw new IOException("Unknown instruction: "+opcode);
			} // switch
		} else throw new IOException("Unknown opcode: 0x"+Integer.toHexString(opcode_no));
    }

    //========== Utility functions ==============================
    public String readString(int len) throws IOException {
		return new String(readBinary(len));
    }
    public byte[] readBinary(int len) throws IOException {
		byte[] data = new byte[len];
		in.readFully(data);
		return data;
    }

    //========== Operand tags:
    //TODO: These need to be cleared up. Look at a bit less.
    static final int CODEINT4_TAG = 0;
    static final int INTLIT4_TAG = 1;
    static final int ATOM4_TAG = 2;
    static final int XREG4_TAG = 3;
    static final int YREG4_TAG = 4;
    static final int LABEL4_TAG = 5;
    static final int EXTENDED_TAG = 7;

    static final int CODEINT12_TAG = 8;
    static final int BIGINT_TAG = 9;
    static final int ATOM12_TAG = 10;
    static final int XREG12_TAG = 11;
    static final int YREG12_TAG = 12;
    static final int LABEL12_TAG = 13;
    static final int EXTENDED2_TAG = 7;

    //========== Extended operand tags:
    static final int FLOATLIT_TAG2   = 0;
    static final int SELECTLIST_TAG2 = 1;
    static final int FLOATREG_TAG2   = 2;
    static final int ALLOCLIST_TAG2  = 3;
    static final int LITERAL_TAG2    = 4;

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

    public Literal readLiteral() throws IOException {
		return readOperand().asLiteral();
    }

    public Atom readAtom() throws IOException {
		return readOperand().asAtom();
    }

    public BitString readBitstringRef() throws IOException {
		int bits  = readCodeInteger();
		int start = readCodeInteger();
		return new BitString(bitstring(start,bits));
    }
    public ByteString readBytestringRef() throws IOException {
		int bytes  = readCodeInteger();
		int start = readCodeInteger();
		return new ByteString(string(start,bytes));
    }

    public SelectList readSelectList() throws IOException {
		return readOperand().asSelectList();
    }

    public AllocList readAllocList() throws IOException {
		switch (peekTag()) {
		case CODEINT4_TAG:
		case CODEINT12_TAG:
		{
			int words = readCodeInteger();
			return new AllocList(words);
		}
		case EXTENDED_TAG: {
			return readOperand().asAllocList();
		}
		default:
			throw new IOException("Expected alloc list, got "+readOperand().toSymbolic());
		} // switch

    }

    public YReg readYReg() throws IOException {
		return readOperand().asYReg();
    }

    public int readCodeInteger() throws IOException {
		int d1 = in.read1();
		int tag = d1 & 0x07;
		if (tag == CODEINT4_TAG)
			return readSmallIntValue(d1);
		else
			throw new IOException("Not a code-int: "+readOperand(d1).toSymbolic());
    }

    public Operand readOperand() throws IOException {
		int d1 = in.read1();
		return readOperand(d1);
    }
    public Operand readOperand(int d1) throws IOException {
		int tag = d1 & 0x07;
		switch (tag) {
		case CODEINT4_TAG:
			return new Operands.CodeInt(readSmallIntValue(d1));

		case INTLIT4_TAG: {
			if ((d1 & 0x8) == 0)
				return new Operands.Int(readSmallIntValue(d1));
			else { // case BIGINT_TAG:
				int hdata  = d1>>4;
				if ((hdata & 1) == 0) { // Fixed-length
					return new Operands.Int((hdata << 7) + in.read1());
				} else {
					int len;
					if (hdata < 15) { // Small var-length
						len = 2+(hdata>>1);
					} else { // Big var-length
						len = 2+(hdata>>1) + readCodeInteger();
					}
					byte d[] = new byte[len];
					in.readFully(d);
					return Operands.makeInt(d);
				}
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
			case FLOATLIT_TAG2:{
				double value = Double.longBitsToDouble(in.readBE(8));
				return new Operands.Float(value);
			}
			case SELECTLIST_TAG2: {
				int length = readCodeInteger();
				assert(length % 2 == 0);
				Operand[] list = new Operand[length];
				for (int i=0; i<length; ) {
					list[i++] = readOperand();
					list[i++] = readLabel();
				}
				return new SelectList(list);
			}
			case ALLOCLIST_TAG2: {
				int length = readCodeInteger();
				int[] list = new int[2*length];
				for (int i=0; i<length; i++) {
					list[2*i] = readCodeInteger();
					list[2*i+1] = readCodeInteger();
				}
				return new AllocList(list);
			}
			case FLOATREG_TAG2: {
				int nr = readSmallIntValue(in.read1());
				return new FReg(nr);
			}
			case LITERAL_TAG2: {
				int nr = readSmallIntValue(in.read1());
				return new TableLiteral(literal(nr));
			}
			default:
				System.err.println("*** Unhandled extended operand tag: "+moretag);
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
		} else if ((hdata & 1) == 0) { // 1 byte more
			return (hdata<<7) + in.read1();
		} else {		       // >1 bytes more
			int len = 2+(hdata>>1);
			byte d[] = new byte[len];
			in.readFully(d);
			BigInteger value = new BigInteger(d);
			if (len>4 || value.compareTo(BigInteger.ZERO) < 0)
				throw new IOException("Code integer out of bounds: "+value);
			else
				return value.intValue();
		}
    }

	static class SectionMetadata {
		final int tag, offset, length;
		public SectionMetadata(int tag, int offset, int length) {
			this.tag = tag;
			this.offset = offset;
			this.length = length;
		}
	}
}

