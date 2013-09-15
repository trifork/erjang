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
import java.lang.Character.UnicodeBlock;
import java.lang.Character.UnicodeScript;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import erjang.ETuple4;
import erjang.NotImplemented;
import erjang.CharCollector.CollectingException;
import erjang.CharCollector.InvalidElementException;
import erjang.CharCollector.PartialDecodingException;
import erjang.driver.IO;

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
			// e.printStackTrace();
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
		Options o2;
		
		if (o.isNil()) {
			o2 = regex.options;
		} else {
			o2 = regex.options.re_init(o);
			
			if (o2 == null) {
				throw ERT.badarg(subj, re, opts);
			}
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
			
			ESeq result = ERT.NIL;
			
			while (matcher.find()) {
				MatchResult mr = matcher.toMatchResult();
			
				ESeq list;
				if (o2.capture_spec == am_all) {
					ESeq l = ERT.NIL;
					for (int i = mr.groupCount(); i >= 0; i--) {
						l = l.cons( capture (subject, mr, i, o2) );
					}
					
					result = result.cons(l);
				} else if ((list = o2.capture_spec.testSeq()) != null) {
					ESeq l = ERT.NIL;
					while (!list.isNil()) {
						EObject group = list.head();
						ESmall num;
						EAtom nam;
						EString nam2;
						if ((num=group.testSmall()) != null)
						{
							l = l.cons( capture (subject, mr, num.value, o2 ));
						} else if ((nam=group.testAtom()) != null) {
							Integer groupNo = o2.named_groups.get(nam.getName());
							if (groupNo != null) {
								l = l.cons( capture (subject, mr, groupNo.intValue(), o2 ));
							}
						} else if ((nam2=group.testString()) != null) {
							Integer groupNo = o2.named_groups.get(nam2.stringValue());
							if (groupNo != null) {
								l = l.cons( capture (subject, mr, groupNo.intValue(), o2 ));
							}
						} else {
							throw new NotImplemented("named capture groups");
						}
						list = list.tail();
					}
					result = result.cons(l);

				} else {
					throw new NotImplemented("global and not all");
				}
			}
			
			if (result == ERT.NIL) {
				return am_nomatch;
			} else {
				 return new ETuple2(am_match, result.reverse());
			}
			
		} else {
			
			if (matcher.find()) {
				
				if (o2.capture_spec  == am_none) {
					return am_match;
				} 
				
				MatchResult mr = matcher.toMatchResult();
			
				int max = mr.groupCount();
				while( mr.start(max) == -1)
					max -= 1;
				
				ESeq il;
				if (o2.capture_spec == am_all) {
					ESeq l = ERT.NIL;
					for (int i = max; i >= 0; i--) {
						l = l.cons( capture (subject, mr, i, o2) );
					}
					return new ETuple2(am_match, l);
					
				} else if (o2.capture_spec == am_all_but_first) {
					ESeq l = ERT.NIL;
					for (int i = max; i > 0; i--) {
						l = l.cons( capture (subject, mr, i, o2) );
					}
					return new ETuple2(am_match, l);
					
				} else if (o2.capture_spec == am_first) {
					EObject l = capture (subject, mr, 0, o2);
					return new ETuple2(am_match, l);
					
				} else if ((il = o2.capture_spec.testSeq()) != null) {
					
					ESeq out = ERT.NIL;
					
					 for (; !il.isNil(); il = il.tail()) {
						 EObject what = il.head();
						ESmall idx = what.testSmall();
						 EAtom nam;
						if (idx != null && mr.start(idx.value) != -1) {
							 EObject val = capture (subject, mr, idx.value, o2);
							 out = out.cons(val);
						 } else if ((nam=what.testAtom())!=null) {
							 Integer idx2 = o2.named_groups.get(nam.getName());
							 if (idx2 != null) {
									 EObject val = capture (subject, mr, idx2, o2);
									 out = out.cons(val);
							 } else {
								 // badarg?
							 }
						 } else {
							 out = out.cons(nocapture(o2));
						 }
					 }
					 
					 return new ETuple2(am_match, out.reverse());
					
				} else {
					throw ERT.badarg(subj, re, opts);
				}
				
			} else {
				return am_nomatch;
			}
		}
	}

	private static EObject nocapture(Options opts) {
		if (opts.capture_type == am_binary) {
			return EBinary.EMPTY;
		} else if (opts.capture_type == am_list) {
			return ERT.NIL;
		} else if (opts.capture_spec == am_index) {
			return am_nomatch;
		} else {
			throw new InternalError("bad capture_type "+opts.capture_type);
		}
	}
	
	private static EObject capture(String subject, MatchResult mr, int group_index,
			Options opts) {

		int start = mr.start(group_index);
		
		if (start == -1) {
			return nocapture(opts);
		}
		
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

		Map<String,Integer> named_groups = new HashMap<>(3);

		public int group_count;
		
		Options re_init(ESeq opts) {
			Options out;
			try {
				out = (Options) this.clone();
			} catch (CloneNotSupportedException e) {
				throw new InternalError();
			}
			if (!out.init(opts))
				return null;
			
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
					if (val.testSmall() == null && val.testString() == null && val.testAtom() == null)
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

			newline_any |= (newline_lf & newline_cr & newline_crlf);
			
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

			if (io_or_char_list instanceof ECompiledRE) {
				ECompiledRE cr = (ECompiledRE)io_or_char_list;
				return cr.patt.pattern();
			}
			
			String pattern;
			if (unicode) {
				CharArrayWriter out = new CharArrayWriter();
				CharCollector cc = new CharCollector(StandardCharsets.UTF_8, out);
				try {
					ESeq rest = io_or_char_list.collectCharList(cc, ERT.NIL);
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

		public boolean isUnicode() {
			return unicode;
		}
		

		static Pattern NAMED_GROUP = Pattern.compile("\\(\\?<([a-zA-Z0-9_]+)>.*");
		static Pattern PREDEFINED = Pattern.compile(".*(\\\\p\\{(?<name>[a-zA-Z][a-zA-Z0-9]*)\\}).*");
		
		private String countGroups(String pattern) {

			int start = 0;
			StringBuilder sb = new StringBuilder();
			
			boolean in_ch_class = false;
			group_count = 0;
			Matcher namedGroupMatcher = NAMED_GROUP.matcher( pattern );

			int i;

			Matcher prefinedMatcher = PREDEFINED.matcher(pattern);

			for (i = 0; i < pattern.length(); i++) {
				char ch = pattern.charAt(i);
				char ch2;
				if (ch == '\\') {
					
					if (prefinedMatcher.find(i) && prefinedMatcher.start(1) == i) {
						String predefined = prefinedMatcher.group(2);
						String javaPredefName = predefined;
					
						do {
							try {
								UnicodeScript found = UnicodeScript.forName(predefined);
								javaPredefName = "Is" + predefined;
								break;
							} catch (IllegalArgumentException e) {
								// ok
							}
						
							try {
								UnicodeBlock found = UnicodeBlock.forName(predefined);
								javaPredefName = "In" + predefined;
							} catch (IllegalArgumentException e) {
								// ok
							}
							
							
						} while(false);
						
						sb.append( pattern.substring( start, i+3 )).append(javaPredefName).append('}');
						i += predefined.length()+3;
						start = i+1; 
						continue;
					}
					
					i += 1;
					continue;
				} 

				if (ch == '(' && !lookingAt(pattern, i+1, "?:")) {					
					group_count += 1;
					
					if (namedGroupMatcher.find(i)) {
						if (namedGroupMatcher.start() == i) {
							String name = namedGroupMatcher.group(1);
							named_groups.put(name, group_count);
							
							sb.append( pattern.substring(start, i+1) );
							start = i+4+name.length();
							i = start-1;
						}
					}
					
				} else if (ch == '[') {
					if (!in_ch_class) {
						in_ch_class = true;
					} else {
						sb.append( pattern.substring( start, i )).append('\\');
						start = i;
					}
				} else if (ch == ']') {
					in_ch_class = false;
					
				// if seeing "{[^0-9]" then insert escape, as java complains about non-numeric
				} else if (ch == '{' 
						&& i+1 < pattern.length() 
						&& (ch2 = pattern.charAt(i+1)) != '0'
						&& !(ch2 >= '1' && ch2 <= '9')) {
					sb.append( pattern.substring( start, i )).append('\\');
					start = i;					
				}
			}
			
			if (start == 0)
				return pattern;

			sb.append( pattern.substring(start) );
			return sb.toString();
		}

		private static boolean lookingAt(String base, int i, CharSequence find) {
			if (base.length()-i < find.length())
				return false;
			
			for (int off = 0; off < find.length(); off += 1) {
				if (find.charAt(off) != base.charAt(i+off))
					return false;
			}
			
			return true;
		}

		public String process_erl2java(String pattern) {
			return countGroups(pattern);
		}
		
	}
	
	//
	// This is an enormous ugly hack, but since Elixir jut has two special-case
	// regular expressions, we'll encode them specifically into erjang.
	//
	
	// output of BEAM's re:compile(<<"\\(\\?<(?<G>[^>]*)>">>)
	static String ELIXIR_GROUPS_PATTERN = "\\(\\?<(?<G>[^>]*)>";
	static byte[] ELIXIR_GROUPS_PATTERN_BEAM = new byte[] {
		69, 82, 67, 80, 80, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 1, 0, 0, 0, 40, 0, 62, 
		2, 48, 0, 4, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
		1, 71, 0, 93, 0, 21, 27, 40, 27, 63, 27, 60, 94, 0, 7, 0, 1, 43, 62, 84, 
		0, 7, 27, 62, 84, 0, 21, 0, -1, -1, -1
	};
	


	// output of BEAM's re:compile(<<"[.^$*+?()[{\\\|\s#]">>, [unicode]).
	static String ELIXIR_ESCAPE_PATTERN = "[\\.\\^\\$\\*\\+\\?\\(\\)\\[\\{\\\\\\|\\s\\#]";
	static byte[] ELIXIR_ESCAPE_PATTERN_BEAM = new byte[] {
        69, 82, 67, 80, 88, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 93, 0, 
        36, 77, 0, 54, 0, 0, 25, 79, 0, -128, 0, 0, 0, 88, 0, 0, 0, 24, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 84, 0, 36, 0
	};
	
	static String is_special_pattern(byte[] raw, Options o) {
		if (Arrays.equals(raw, ELIXIR_ESCAPE_PATTERN_BEAM)) {
			o.unicode = true;
			return ELIXIR_ESCAPE_PATTERN;
		}
		
		if (raw.length == ELIXIR_GROUPS_PATTERN_BEAM.length) {
			for (int i = 0; i < raw.length-4; i++) {
				if (raw[i] != ELIXIR_GROUPS_PATTERN_BEAM[i])
					return null;
			}
		} else {
			return null;
		}
	
		return ELIXIR_GROUPS_PATTERN;
	}
	
	@BIF
	static public ETuple2 compile(EObject obj1, EObject obj2) {

		if (obj1 instanceof ECompiledRE) {
			return new ETuple2(ERT.am_ok,
					obj1
			);
		}
		
		ESeq opts = obj2.testSeq();

		Options o = new Options();

		String pattern;
		ETuple4 tup = ETuple4.cast(obj1);
		if (tup != null && tup.elem1 == ECompiledRE.am_re_pattern) {
			EBinary b = tup.elem4.testBinary();
			byte[] byteArray = b.getByteArray();
			if (b != null && b.byteAt(0) == '/') {
				
				byte[] raw = byteArray;
				int end = raw.length - 1;
				for (int i = b.byteSize()-1; i > 0; i--) {
					if (b.byteAt(i*8) == '/') {
						end = i;
						break;
					}
				}	
					
				pattern = new String(raw, 1, end-1, IO.UTF8);
	
				o.init( ECompiledRE.decode_options(raw, end+1) );
				
				if (!o.init(opts)) {
					throw ERT.badarg(obj1, obj2);
				}

			} else if ((pattern = is_special_pattern(byteArray, o)) != null) {
				// ok //
				
			} else {
				System.out.println("byte data[] = { ");
				for (int i = 0; i < byteArray.length; i++) {
					System.out.print(", "+byteArray[i]);
				}
				System.out.println(" } ");
				throw ERT.badarg(obj1, obj2);
			}
		} else {
			if (!o.init(opts)) {
				throw ERT.badarg(obj1, obj2);
			}
			pattern = o.decode(obj1);
		}
		
		if (pattern == null) {
			throw ERT.badarg(obj1, obj2);
		}
		
		String stripped_pattern = o.process_erl2java(pattern);

		try {
			Pattern c = Pattern.compile(stripped_pattern, o.flags);
			return new ETuple2(ERT.am_ok,
					new ECompiledRE(o, c, pattern)
			);

		} catch (PatternSyntaxException e) {
			return new ETuple2(ERT.am_error, new ETuple2(EString.fromString(e
					.getDescription()+" in /"+stripped_pattern+"/"), ERT.box(e.getIndex())));
		}
	}
	

}
