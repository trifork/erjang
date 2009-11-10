grammar beam;

options {
  language = Java;
}

@header {
   package org.erlang.jbeam; 
   import org.erlang.*;
}

@lexer::header {
   package org.erlang.jbeam; 
   import org.erlang.*;
}

@members {
      public static List<ETerm>   parse(java.io.Reader reader) throws Exception {
        ANTLRReaderStream rs = new ANTLRReaderStream(reader);
        beamLexer lex = new beamLexer(rs);
        CommonTokenStream cts = new CommonTokenStream(lex);
        beamParser par = new beamParser(cts, null);
        return par.beamFile();
      }
      
      public static void main(String[] args) throws Exception {
         List<ETerm>  s = parse(new java.io.FileReader("m.dis"));
         new BEAMFile().init(s);
         System.out.println(s);
      }
      
          static ENumber integer(String str) throws RuntimeException {
            return ENumber.parseInt(str);
          }

}

beamFile returns[List<ETerm> l]
@init { List<ETerm> list = new ArrayList<ETerm>(); }
:
  (stmt=term '.' { list.add(stmt); })+ 
  { l = list; }
;
  
term returns[ETerm t]:
    o=touple { t = o; }
  | l=list { t = l; }
  | atom=Atom { t = EAtom.intern($atom.text); }
  | string=String { t = new EString($string.text); }
  | integer=Integer { t = integer($integer.text); }
  | real=Float { t = EDouble.parseDouble($real.text); }
  ;
  
list returns[ESeq res]
@init{ res = ECons.EMPTY; ESeq tt = ECons.EMPTY; }
: '[' 
    ( h=term (',' t=tail { tt=t; })?
       { res = tt.cons(h); }
    )?
   ']';

tail returns[ESeq res] 
@init { res=ECons.EMPTY; }
:
    h=term 
    (',' t=tail { res=t.cons(h); }
    | { res = res.cons(h); }
    )
;

touple returns[ETuple tup]
@init{ List<ETerm> elms = new ArrayList<ETerm>(); }
: '{' 
   (   t1=term { elms.add(t1); }
  (',' t2=term { elms.add(t2); } )*  
   )?
  '}'
  { tup = ETuple.make(elms.toArray(new ETerm[elms.size()])); }
  ;

Atom: ('a'..'z' ( 'a'..'z' | 'A'..'Z' | Digit | '@' | '_' )*)
    | ('\'' body=AtomBody '\'' { state.text = $body.text; })
    ;

fragment AtomBody: AtomChar*;

fragment Digit: '0'..'9';

fragment AtomChar: 
      '\\' Digit Digit Digit
    | '\\' ~Digit
    | ~('\\' | '\'')
    ;

Float: '-'? Digit+ '.' Digit+ ('e' Digit+)?;    
Integer: '-'? Digit+;

String: '"' body=StringBody '"' { state.text = $body.text; };

fragment StringBody: StringChar*;

fragment StringChar:
     '\\' Digit Digit Digit
   | '\\' ~Digit
   | ~('\\' | '"');

WS: (' ' | '\t' | '\n') { $channel = HIDDEN; };
