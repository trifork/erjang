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

public enum BeamOpcode {

	label,
	func_info,
	int_code_end,
	call,
	call_last,
	call_only,
	call_ext,
	call_ext_last,
	bif0,
	bif1,
	bif2,
	allocate,
	allocate_heap,
	allocate_zero,
	allocate_heap_zero,
	test_heap,
	init,
	deallocate,
	K_return,
	send,
	remove_message,
	timeout,
	loop_rec,
	loop_rec_end,
	wait,
	wait_timeout,
	is_lt,
	is_ge,
	is_eq,
	is_ne,
	is_eq_exact,
	is_ne_exact,
	is_integer,
	is_float,
	is_number,
	is_atom,
	is_pid,
	is_reference,
	is_port,
	is_nil,
	is_binary,
	is_list,
	is_nonempty_list,
	is_tuple,
	test_arity,
	select_val,
	select_tuple_arity,
	jump,
	K_catch,
	catch_end,
	move,
	get_list,
	get_tuple_element,
	set_tuple_element,
	put_string,
	put_list,
	put_tuple,
	put,
	badmatch,
	if_end,
	case_end,
	call_fun,
	is_function,
	call_ext_only,
	bs_put_integer,
	bs_put_binary,
	bs_put_float,
	bs_put_string,
	fclearerror,
	fcheckerror,
	fmove,
	fconv,
	fadd,
	fsub,
	fmul,
	fdiv,
	fnegate,
	make_fun2,
	K_try,
	try_end,
	try_case,
	try_case_end,
	raise,
	bs_init2,
	bs_bits_to_bytes,
	bs_add,
	apply,
	apply_last,
	is_boolean,
	is_function2,
	bs_start_match2,
	bs_get_integer2,
	bs_get_float2,
	bs_get_binary2,
	bs_skip_bits2,
	bs_test_tail2,
	bs_save2,
	bs_restore2,
	gc_bif1,
	gc_bif2,
	is_bitstr,
	bs_context_to_binary,
	bs_test_unit,
	bs_match_string,
	bs_init_writable,
	bs_append,
	bs_private_append,
	trim,
	bs_init_bits,
	bs_get_utf8,
	bs_skip_utf8,
	bs_get_utf16,
	bs_skip_utf16,
	bs_get_utf32,
	bs_skip_utf32,
	bs_utf8_size,
	bs_put_utf8,
	bs_utf16_size,
	bs_put_utf16,
	bs_put_utf32,
	
	//
	
	test, bif, gc_bif, arithfbif,
	
	// illegal op-code
	NONE
	
	;

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
