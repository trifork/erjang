/**
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
package erjang.m.re;

import java.io.CharArrayWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import erjang.BIF;
import erjang.CharCollector;
import erjang.EAtom;
import erjang.EBigString;
import erjang.EBinary;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.NotImplemented;
import erjang.CharCollector.CollectingException;
import erjang.CharCollector.InvalidElementException;
import erjang.CharCollector.PartialDecodingException;

public class Native extends ENative {
	
	/** Urgh! Returned index values needs to represent the index into the 
	 * underlying UTF8 byte stream, if unicode is assumed... */
	private static final boolean INDEX_COMPATIBLE = true;
	
	public static EAtom am_nomatch = EAtom.intern("nomatch");
	public static EAtom am_match = EAtom.intern("match");
	public static EAtom am_latin1  = EAtom.intern("latin1");

	public static EAtom am_unicode = EAtom.intern("unicode");
	public static EAtom am_anchored = EAtom.intern("anchored");
	public static EAtom am_caseless = EAtom.intern("caseless");
	public static EAtom am_dollar_endonly = EAtom.intern("dollar_endonly");
	public static EAtom am_dotall = EAtom.intern("dotall");
	public static EAtom am_extended = EAtom.intern("extended");
	public static EAtom am_firstline = EAtom.intern("firstline");
	public static EAtom am_multiline = EAtom.intern("multiline");
	public static EAtom am_no_auto_capture = EAtom.intern("no_auto_capture");
	public static EAtom am_dupnames = EAtom.intern("dupnames");
	public static EAtom am_ungreedy = EAtom.intern("ungreedy");
	public static EAtom am_newline = EAtom.intern("newline");
	public static EAtom am_bsr_anycrlf = EAtom.intern("bsr_anycrlf");
	public static EAtom am_bsr_unicode = EAtom.intern("bsr_unicode");

	public static EAtom am_cr = EAtom.intern("cr");
	public static EAtom am_lf = EAtom.intern("lf");
	public static EAtom am_crlf = EAtom.intern("crlf");
	public static EAtom am_anycrlf = EAtom.intern("anycrlf");
	public static EAtom am_any = EAtom.intern("any");

	public static EAtom am_global = EAtom.intern("global");
	public static EAtom am_none = EAtom.intern("none");
	public static EAtom am_index = EAtom.intern("index");
	public static EAtom am_binary = EAtom.intern("binary");
	public static EAtom am_list = EAtom.intern("list");
	public static EAtom am_all = EAtom.intern("all");
	public static EAtom am_first = EAtom.intern("first");
	public static EAtom am_all_but_first = EAtom.intern("all_but_first");
	public static EAtom am_capture = EAtom.intern("capture");
	public static EAtom am_offset = EAtom.intern("offset");

	@BIF
	public static EObject run(EObject subject, EObject pattern) {
		return run(subject, pattern, ERT.NIL);
	}

	@BIF
	static public EObject run(EObject subj, EObject re, EObject opts) {
		try {
			EObject res = run2(subj, re, opts);
			//System.out.println("re:run("+subj+", "+re+", "+opts+") => "+res);
			return res;
		} catch (RuntimeException e) {
			//System.out.println("re:run("+subj+", "+re+", "+opts+") => "+e);
			e.printStackTrace();
			throw e;
		}
	}
	
	static public EObject run2(EObject subj, EObject re, EObject opts) {
		ECompiledRE regex;

		if (re instanceof ECompiledRE) {
			regex = (ECompiledRE) re;

		} else {
			ETuple2 res = compile(re, opts);
			EObject val = res.elem2;
			if (res.elem1 == ERT.am_ok && ( val instanceof ECompiledRE)) {
				regex = (ECompiledRE) val;
			} else {
				return val;
			}
		}

		ESeq o = opts.testSeq();
		Options o2 = new Options();
		if (!o2.init(o)) {
			throw ERT.badarg(subj, re, opts);
		}
		
		String subject = regex.options.decode(subj);
		if (subject == null) {
			throw ERT.badarg(subj, re, opts);
		}
		
		if (o2.offset > subject.length() || o2.offset < 0) {
			throw ERT.badarg(subj, re, opts);
		}
		
		Matcher matcher = regex.patt.matcher(subject.substring(o2.offset ));
				
		if (o2.global) {
			
			throw new NotImplemented("global option to re:run");
			
		} else {
			
			if (matcher.find()) {
				
				if (o2.capture_spec  == am_none) {
					return am_match;
				} 
				
				MatchResult mr = matcher.toMatchResult();
			
				ESeq il;
				if (o2.capture_spec == am_all) {
					ESeq l = ERT.NIL;
					for (int i = mr.groupCount(); i >= 0; i--) {
						l = l.cons( capture (subject, mr, i, o2) );
					}
					return new ETuple2(am_match, l);
					
				} else if (o2.capture_spec == am_all_but_first) {
					ESeq l = ERT.NIL;
					for (int i = mr.groupCount(); i > 0; i--) {
						l = l.cons( capture (subject, mr, i, o2) );
					}
					return new ETuple2(am_match, l);
					
				} else if (o2.capture_spec == am_first) {
					EObject l = capture (subject, mr, 0, o2);
					return new ETuple2(am_match, l);
					
				} else if ((il = o2.capture_spec.testSeq()) != null) {
					
					ESeq out = ERT.NIL;
					
					 for (; !il.isNil(); il = il.tail()) {
						 ESmall idx = il.head().testSmall();
						 EObject val = capture (subject, mr, idx.value, o2);
						 out = out.cons(val);
					 }
					 
					 return new ETuple2(am_match, out);
					
				} else {
					throw ERT.badarg(subj, re, opts);
				}
				
			} else {
				return am_nomatch;
			}
		}
	}

	private static EObject capture(String subject, MatchResult mr, int group_index,
			Options opts) {

		int start = mr.start(group_index);
		int end = mr.end(group_index);

		if (opts.capture_type  == am_index) {
			
			if (INDEX_COMPATIBLE && opts.unicode) {
				try {
					int istart = subject.substring(0, start).getBytes("UTF8").length;
					int ilen = subject.substring(start, end).getBytes("UTF8").length;

					return new ETuple2(ERT.box(istart), ERT.box(ilen));
				} catch (UnsupportedEncodingException e) {
					throw new InternalError();
				}
			}
			
			return new ETuple2(ERT.box(start), ERT.box(end-start));
			
		} else if (opts.capture_type == am_list) {
			String sub = subject.substring(start, end);
			EBigString ebs = EBigString.fromString(sub);
			return erjang.m.unicode.Native.characters_to_list(ebs, 
						opts.unicode ? am_unicode : am_latin1);
			
		} else if (opts.capture_type == am_binary) {
			String sub = subject.substring(start, end);
			EBigString ebs = EBigString.fromString(sub);
			return erjang.m.unicode.Native.characters_to_binary(ebs, 
						opts.unicode ? am_unicode : am_latin1);
		} else {
			throw new InternalError("bad capture type: "+opts.capture_type);
		}
		
	}

	@BIF
	static public EObject compile(EObject obj1) {
		return compile(obj1, ERT.NIL);
	}

	static class Options implements java.lang.Cloneable{

		public int offset = 0;

		public EObject capture_type = am_index;

		public EObject capture_spec = am_all;

		public boolean global;

		boolean unicode = false;

		boolean newline_cr = false;
		boolean newline_lf = true;
		boolean newline_crlf = false;
		boolean newline_any = false;

		int flags = 0;

		boolean anchored = true;

		Options re_init(ESeq opts) {
			Options out;
			try {
				out = (Options) this.clone();
			} catch (CloneNotSupportedException e) {
				throw new InternalError();
			}
			out.init(opts);
			return out;
		}
		
		boolean init(ESeq opts) {
			
			if (opts == null) return true;

			for (; !opts.isNil(); opts = opts.tail()) {

				EObject opt = opts.head();

				// unicode | anchored | caseless | dollar_endonly | dotall |
				// extended
				// | firstline | multiline | no_auto_capture | dupnames |
				// ungreedy
				// | {newline, NLSpec}| bsr_anycrlf | bsr_unicode

				ETuple tup;
				ESmall off;
				if (opt == am_unicode) {
					unicode = true;
				} else if (opt == am_anchored) {
					anchored = true;
				} else if (opt == am_global) {
					global = true;
				} else if (opt == am_caseless) {
					flags |= Pattern.CASE_INSENSITIVE;
				} else if (opt == am_dollar_endonly) {
					throw new NotImplemented("regex option "+opt);
				} else if (opt == am_dotall) {
					flags |= Pattern.DOTALL;
				} else if (opt == am_extended) {
					flags |= Pattern.COMMENTS;
				} else if (opt == am_firstline) {
					throw new NotImplemented("regex option "+opt);
				} else if (opt == am_multiline) {
					flags |= Pattern.MULTILINE;
				} else if (opt == am_no_auto_capture) {
					throw new NotImplemented("regex option "+opt);
				} else if (opt == am_dupnames) {
					throw new NotImplemented("regex option "+opt);
				} else if (opt == am_ungreedy) {
					throw new NotImplemented("regex option "+opt);
				} else if (opt == am_bsr_anycrlf) {
					newline_cr = true;
					newline_crlf = true;
					newline_lf = true;
					newline_any = false;

				} else if (opt == am_bsr_unicode) {
					newline_any = true;

				} else if ((tup = opt.testTuple()) != null && tup.arity() == 2
						&& tup.elm(1) == am_newline) {

					newline_cr = false;
					newline_crlf = false;
					newline_lf = false;
					newline_any = false;

					EObject val = tup.elm(2);
					if (val == am_cr) {
						newline_cr = true;
					} else if (val == am_lf) {
						newline_lf = true;
					} else if (val == am_crlf) {
						newline_crlf = true;
					} else if (val == am_anycrlf) {
						newline_cr = true;
						newline_lf = true;
						newline_crlf = true;
					} else if (val == am_any) {
						newline_any = true;
					} else {
						return false;
					}
					
				} else if (tup != null && tup.arity() == 2 && tup.elm(1) == am_capture) {
					this.capture_spec = tup.elm(2);
					this.capture_type = am_index;

				} else if (tup != null && tup.arity() == 3 && tup.elm(1) == am_capture) {
					this.capture_spec = tup.elm(2);
					this.capture_type = tup.elm(3);

				} else if (tup != null && tup.arity() == 2 
						&& tup.elm(1) == am_offset
						&& (off=tup.elm(2).testSmall()) != null) {
					this.offset = off.value;

				} else {
					return false;
				}

			}
			
			ESeq spec;
			if (capture_spec == am_all 
				|| capture_spec == am_all_but_first
				|| capture_spec == am_first
				|| capture_spec == am_none
				) {
				// ok
			} else if ((spec=capture_spec.testSeq()) != null) {
				
				// if it is a sequence, make sure elements are integers
				while (!spec.isNil()) {
					EObject val = spec.head();
					if (val.testSmall() == null)
						return false;
					spec = spec.tail();
				}
				
				// ok
			} else {	
				return false;
			}
			
			if (capture_type == am_index
				|| capture_type == am_list 
				|| capture_type == am_binary) {
				// ok
			} else {
				return false;
			}

			if (unicode == true && ((flags & Pattern.CASE_INSENSITIVE) != 0)) {
				flags |= Pattern.UNICODE_CASE;
			}

			if (newline_any == true) {
				// great, this is the Java default

			} else if (newline_lf == true && newline_cr == false
					&& newline_crlf == false) {
				flags |= Pattern.UNIX_LINES;

			} else {
				// TODO: this combination not supported by Java.
				throw new NotImplemented("regex newline options lf="
						+ newline_lf + "; cr=" + newline_cr + "; crlf="
						+ newline_crlf);
			}

			return true;
		}

		String decode(EObject io_or_char_list) {

			String pattern;
			if (unicode) {
				CharArrayWriter out = new CharArrayWriter();
				Charset spec = Charset.forName("UTF-16BE");
				CharCollector cc = new CharCollector(spec, out);
				try {
					io_or_char_list.collectCharList(cc);
					cc.end();
				} catch (CollectingException e) {
					return null;
				} catch (InvalidElementException e) {
					return null;
				} catch (IOException e) {
					return null;
				} catch (PartialDecodingException e) {
					return null;
				}
				pattern = out.toString();
			} else {
				
				EBinary bin;
				if ((bin = io_or_char_list.testBinary()) != null) {
					return EString.make(bin).stringValue();
				}
				
				EString str;
				if ((str = io_or_char_list.testString()) != null) {
					return str.stringValue();
				}
				
				List<ByteBuffer> bb = new ArrayList<ByteBuffer>();
				if (io_or_char_list.collectIOList(bb)) {
					StringWriter sw = new StringWriter();
					for (ByteBuffer b : bb) {
						char ch;

						while (b.hasRemaining()) 
						{
							ch = (char) b.get();
							sw.append(ch);
						}
					}

					pattern = sw.toString();
				} else {
					return null;
				}
			}

			return pattern;
		}
	}

	@BIF
	static public ETuple2 compile(EObject obj1, EObject obj2) {

		ESeq opts = obj2.testSeq();

		Options o = new Options();
		if (!o.init(opts)) {
			throw ERT.badarg(obj1, obj2);
		}

		String pattern = o.decode(obj1);

		try {
			Pattern c = Pattern.compile(pattern, o.flags);
			return new ETuple2(ERT.am_ok,
					new ECompiledRE(o, c)
			);

		} catch (PatternSyntaxException e) {
			return new ETuple2(ERT.am_error, new ETuple2(EString.fromString(e
					.getDescription()), ERT.box(e.getIndex())));
		}
	}
	

}
