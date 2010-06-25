/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2009 by Trifork
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

package erjang.beam;

import erjang.EAtom;
import java.util.EnumSet;

public enum BeamOpcode {

	label			(0x01),
	func_info		(0x02),
	int_code_end		(0x03),
	call			(0x04),
	call_last		(0x05),
	call_only		(0x06),
	call_ext		(0x07),
	call_ext_last		(0x08),
	bif0			(0x09),
	bif1			(0x0a),
	bif2			(0x0b),
	allocate		(0x0c),
	allocate_heap		(0x0d),
	allocate_zero		(0x0e),
	allocate_heap_zero	(0x0f),
	test_heap		(0x10),
	init			(0x11),
	deallocate		(0x12),
	K_return		(0x13),
	send                    (0x14),
	remove_message		(0x15),
	timeout                 (0x16),
	loop_rec		(0x17),
	loop_rec_end		(0x18),
	wait			(0x19),
	wait_timeout		(0x1a),
	is_lt			(0x27),
	is_ge			(0x28),
	is_eq			(0x29),
	is_ne			(0x2a),
	is_eq_exact		(0x2b),
	is_ne_exact		(0x2c),
	is_integer		(0x2d),
	is_float		(0x2e),
	is_number		(0x2f),
	is_atom			(0x30),
	is_pid			(0x31),
	is_reference		(0x32),
	is_port			(0x33),
	is_nil			(0x34),
	is_binary		(0x35),
	is_list			(0x37),
	is_nonempty_list	(0x38),
	is_tuple		(0x39),
	test_arity		(0x3a),
	select_val		(0x3b),
	select_tuple_arity	(0x3c),
	jump			(0x3d),
	K_catch			(0x3e),
	catch_end		(0x3f),
	move			(0x40),
	get_list		(0x41),
	get_tuple_element	(0x42),
	set_tuple_element	(0x43),
	put_string		(0x44),
	put_list                (0x45),
	put_tuple		(0x46),
	put			(0x47),
	badmatch		(0x48),
	if_end			(0x49),
	case_end		(0x4a),
	call_fun		(0x4b),
	is_function		(0x4d),
	call_ext_only		(0x4e),
	bs_put_integer		(0x59),
	bs_put_binary		(0x5a),
	bs_put_float		(0x5b),
	bs_put_string		(0x5c),
	fclearerror		(0x5e),
	fcheckerror		(0x5f),
	fmove			(0x60),
	fconv			(0x61),
	fadd			(0x62),
	fsub			(0x63),
	fmul			(0x64),
	fdiv			(0x65),
	fnegate			(0x66),
	make_fun2		(0x67),
	K_try			(0x68),
	try_end			(0x69),
	try_case		(0x6a),
	try_case_end		(0x6b),
	raise			(0x6c),
	bs_init2		(0x6d),
	bs_bits_to_bytes	(0x6e),
	bs_add			(0x6f),
	apply			(0x70),
	apply_last		(0x71),
	is_boolean		(0x72),
	is_function2		(0x73),
	bs_start_match2		(0x74),
	bs_get_integer2		(0x75),
	bs_get_float2		(0x76),
	bs_get_binary2		(0x77),
	bs_skip_bits2		(0x78),
	bs_test_tail2		(0x79),
	bs_save2		(0x7a),
	bs_restore2		(0x7b),
	gc_bif1			(0x7c),
	gc_bif2			(0x7d),
	is_bitstr		(0x81),
	bs_context_to_binary    (0x82),
	bs_test_unit		(0x83),
	bs_match_string		(0x84),
	bs_init_writable	(0x85),
	bs_append		(0x86),
	bs_private_append	(0x87),
	trim                    (0x88),
	bs_init_bits		(0x89),
	bs_get_utf8		(0x8a),
	bs_skip_utf8		(0x8b),
	bs_get_utf16		(0x8c),
	bs_skip_utf16		(0x8d),
	bs_get_utf32		(0x8e),
	bs_skip_utf32		(0x8f),
	bs_utf8_size		(0x90),
	bs_put_utf8		(0x91),
	bs_utf16_size		(0x92),
	bs_put_utf16		(0x93),
	bs_put_utf32		(0x94),
	on_load				(0x95),
	
	recv_mark           (0x96),
	recv_set            (0x97),
	
	// Opcode groups
	
	test,
	bif,
	gc_bif,
	arithfbif,
	
	// illegal op-code
	NONE
	;

	public final EAtom symbol = EAtom.intern(name().startsWith("K_")
						 ? name().substring(2)
						 : name());

	public final int encoding;
	BeamOpcode() {this.encoding = -1;}
	BeamOpcode(int encoding) {this.encoding = encoding;}

	static BeamOpcode[] decodeMap;
	static {
		decodeMap = new BeamOpcode[256];
		for (BeamOpcode x : EnumSet.allOf(BeamOpcode.class))
			if (x.encoding >= 0) decodeMap[x.encoding] = x;
	}
	public static BeamOpcode decode(int opcode) {return decodeMap[opcode];}


	static EAtom TRY = EAtom.intern("try");
	static EAtom CATCH = EAtom.intern("catch");
	static EAtom RETURN = EAtom.intern("return");
	
	static public BeamOpcode get(EAtom sym) {
		String name = sym.getName();
		if (sym == TRY) return K_try;
		if (sym == CATCH) return K_catch;
		if (sym == RETURN) return K_return;
		
		return valueOf(name);
	}


}
