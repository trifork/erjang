// $ANTLR 3.1.2 /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g 2009-11-05 21:55:22

   package org.erlang.jbeam; 
   import org.erlang.*;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class beamParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "Atom", "String", "Integer", "Float", "Digit", "AtomBody", "AtomChar", "StringBody", "StringChar", "WS", "'.'", "'['", "','", "']'", "'{'", "'}'"
    };
    public static final int AtomChar=10;
    public static final int Atom=4;
    public static final int Digit=8;
    public static final int EOF=-1;
    public static final int StringBody=11;
    public static final int Float=7;
    public static final int T__19=19;
    public static final int T__16=16;
    public static final int WS=13;
    public static final int T__15=15;
    public static final int T__18=18;
    public static final int T__17=17;
    public static final int T__14=14;
    public static final int StringChar=12;
    public static final int String=5;
    public static final int AtomBody=9;
    public static final int Integer=6;

    // delegates
    // delegators


        public beamParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public beamParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return beamParser.tokenNames; }
    public String getGrammarFileName() { return "/Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g"; }


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




    // $ANTLR start "beamFile"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:38:1: beamFile returns [List<ETerm> l] : (stmt= term '.' )+ ;
    public final List<ETerm> beamFile() throws RecognitionException {
        List<ETerm> l = null;

        ETerm stmt = null;


         List<ETerm> list = new ArrayList<ETerm>(); 
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:40:1: ( (stmt= term '.' )+ )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:41:3: (stmt= term '.' )+
            {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:41:3: (stmt= term '.' )+
            int cnt1=0;
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( ((LA1_0>=Atom && LA1_0<=Float)||LA1_0==15||LA1_0==18) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:41:4: stmt= term '.'
            	    {
            	    pushFollow(FOLLOW_term_in_beamFile58);
            	    stmt=term();

            	    state._fsp--;

            	    match(input,14,FOLLOW_14_in_beamFile60); 
            	     list.add(stmt); 

            	    }
            	    break;

            	default :
            	    if ( cnt1 >= 1 ) break loop1;
                        EarlyExitException eee =
                            new EarlyExitException(1, input);
                        throw eee;
                }
                cnt1++;
            } while (true);

             l = list; 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return l;
    }
    // $ANTLR end "beamFile"


    // $ANTLR start "term"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:45:1: term returns [ETerm t] : (o= touple | l= list | atom= Atom | string= String | integer= Integer | real= Float );
    public final ETerm term() throws RecognitionException {
        ETerm t = null;

        Token atom=null;
        Token string=null;
        Token integer=null;
        Token real=null;
        ETuple o = null;

        ESeq l = null;


        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:45:22: (o= touple | l= list | atom= Atom | string= String | integer= Integer | real= Float )
            int alt2=6;
            switch ( input.LA(1) ) {
            case 18:
                {
                alt2=1;
                }
                break;
            case 15:
                {
                alt2=2;
                }
                break;
            case Atom:
                {
                alt2=3;
                }
                break;
            case String:
                {
                alt2=4;
                }
                break;
            case Integer:
                {
                alt2=5;
                }
                break;
            case Float:
                {
                alt2=6;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }

            switch (alt2) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:46:5: o= touple
                    {
                    pushFollow(FOLLOW_touple_in_term88);
                    o=touple();

                    state._fsp--;

                     t = o; 

                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:47:5: l= list
                    {
                    pushFollow(FOLLOW_list_in_term98);
                    l=list();

                    state._fsp--;

                     t = l; 

                    }
                    break;
                case 3 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:48:5: atom= Atom
                    {
                    atom=(Token)match(input,Atom,FOLLOW_Atom_in_term108); 
                     t = EAtom.intern((atom!=null?atom.getText():null)); 

                    }
                    break;
                case 4 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:49:5: string= String
                    {
                    string=(Token)match(input,String,FOLLOW_String_in_term118); 
                     t = new EString((string!=null?string.getText():null)); 

                    }
                    break;
                case 5 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:50:5: integer= Integer
                    {
                    integer=(Token)match(input,Integer,FOLLOW_Integer_in_term128); 
                     t = integer((integer!=null?integer.getText():null)); 

                    }
                    break;
                case 6 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:51:5: real= Float
                    {
                    real=(Token)match(input,Float,FOLLOW_Float_in_term138); 
                     t = EDouble.parseDouble((real!=null?real.getText():null)); 

                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return t;
    }
    // $ANTLR end "term"


    // $ANTLR start "list"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:54:1: list returns [ESeq res] : '[' (h= term ( ',' t= tail )? )? ']' ;
    public final ESeq list() throws RecognitionException {
        ESeq res = null;

        ETerm h = null;

        ESeq t = null;


         res = ECons.EMPTY; ESeq tt = ECons.EMPTY; 
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:56:1: ( '[' (h= term ( ',' t= tail )? )? ']' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:56:3: '[' (h= term ( ',' t= tail )? )? ']'
            {
            match(input,15,FOLLOW_15_in_list160); 
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:57:5: (h= term ( ',' t= tail )? )?
            int alt4=2;
            int LA4_0 = input.LA(1);

            if ( ((LA4_0>=Atom && LA4_0<=Float)||LA4_0==15||LA4_0==18) ) {
                alt4=1;
            }
            switch (alt4) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:57:7: h= term ( ',' t= tail )?
                    {
                    pushFollow(FOLLOW_term_in_list171);
                    h=term();

                    state._fsp--;

                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:57:14: ( ',' t= tail )?
                    int alt3=2;
                    int LA3_0 = input.LA(1);

                    if ( (LA3_0==16) ) {
                        alt3=1;
                    }
                    switch (alt3) {
                        case 1 :
                            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:57:15: ',' t= tail
                            {
                            match(input,16,FOLLOW_16_in_list174); 
                            pushFollow(FOLLOW_tail_in_list178);
                            t=tail();

                            state._fsp--;

                             tt=t; 

                            }
                            break;

                    }

                     res = tt.cons(h); 

                    }
                    break;

            }

            match(input,17,FOLLOW_17_in_list203); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return res;
    }
    // $ANTLR end "list"


    // $ANTLR start "tail"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:62:1: tail returns [ESeq res] : h= term ( ',' t= tail | ) ;
    public final ESeq tail() throws RecognitionException {
        ESeq res = null;

        ETerm h = null;

        ESeq t = null;


         res=ECons.EMPTY; 
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:64:1: (h= term ( ',' t= tail | ) )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:65:5: h= term ( ',' t= tail | )
            {
            pushFollow(FOLLOW_term_in_tail226);
            h=term();

            state._fsp--;

            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:66:5: ( ',' t= tail | )
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( (LA5_0==16) ) {
                alt5=1;
            }
            else if ( (LA5_0==17) ) {
                alt5=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }
            switch (alt5) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:66:6: ',' t= tail
                    {
                    match(input,16,FOLLOW_16_in_tail234); 
                    pushFollow(FOLLOW_tail_in_tail238);
                    t=tail();

                    state._fsp--;

                     res=t.cons(h); 

                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:67:7: 
                    {
                     res = res.cons(h); 

                    }
                    break;

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return res;
    }
    // $ANTLR end "tail"


    // $ANTLR start "touple"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:71:1: touple returns [ETuple tup] : '{' (t1= term ( ',' t2= term )* )? '}' ;
    public final ETuple touple() throws RecognitionException {
        ETuple tup = null;

        ETerm t1 = null;

        ETerm t2 = null;


         List<ETerm> elms = new ArrayList<ETerm>(); 
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:73:1: ( '{' (t1= term ( ',' t2= term )* )? '}' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:73:3: '{' (t1= term ( ',' t2= term )* )? '}'
            {
            match(input,18,FOLLOW_18_in_touple270); 
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:74:4: (t1= term ( ',' t2= term )* )?
            int alt7=2;
            int LA7_0 = input.LA(1);

            if ( ((LA7_0>=Atom && LA7_0<=Float)||LA7_0==15||LA7_0==18) ) {
                alt7=1;
            }
            switch (alt7) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:74:8: t1= term ( ',' t2= term )*
                    {
                    pushFollow(FOLLOW_term_in_touple282);
                    t1=term();

                    state._fsp--;

                     elms.add(t1); 
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:75:3: ( ',' t2= term )*
                    loop6:
                    do {
                        int alt6=2;
                        int LA6_0 = input.LA(1);

                        if ( (LA6_0==16) ) {
                            alt6=1;
                        }


                        switch (alt6) {
                    	case 1 :
                    	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:75:4: ',' t2= term
                    	    {
                    	    match(input,16,FOLLOW_16_in_touple289); 
                    	    pushFollow(FOLLOW_term_in_touple293);
                    	    t2=term();

                    	    state._fsp--;

                    	     elms.add(t2); 

                    	    }
                    	    break;

                    	default :
                    	    break loop6;
                        }
                    } while (true);


                    }
                    break;

            }

            match(input,19,FOLLOW_19_in_touple310); 
             tup = ETuple.make(elms.toArray(new ETerm[elms.size()])); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return tup;
    }
    // $ANTLR end "touple"

    // Delegated rules


 

    public static final BitSet FOLLOW_term_in_beamFile58 = new BitSet(new long[]{0x0000000000004000L});
    public static final BitSet FOLLOW_14_in_beamFile60 = new BitSet(new long[]{0x00000000000480F2L});
    public static final BitSet FOLLOW_touple_in_term88 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_list_in_term98 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_Atom_in_term108 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_String_in_term118 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_Integer_in_term128 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_Float_in_term138 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_15_in_list160 = new BitSet(new long[]{0x00000000000780F0L});
    public static final BitSet FOLLOW_term_in_list171 = new BitSet(new long[]{0x0000000000030000L});
    public static final BitSet FOLLOW_16_in_list174 = new BitSet(new long[]{0x00000000000580F0L});
    public static final BitSet FOLLOW_tail_in_list178 = new BitSet(new long[]{0x0000000000020000L});
    public static final BitSet FOLLOW_17_in_list203 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_term_in_tail226 = new BitSet(new long[]{0x0000000000010002L});
    public static final BitSet FOLLOW_16_in_tail234 = new BitSet(new long[]{0x00000000000580F0L});
    public static final BitSet FOLLOW_tail_in_tail238 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_18_in_touple270 = new BitSet(new long[]{0x00000000000D80F0L});
    public static final BitSet FOLLOW_term_in_touple282 = new BitSet(new long[]{0x0000000000090000L});
    public static final BitSet FOLLOW_16_in_touple289 = new BitSet(new long[]{0x00000000000D80F0L});
    public static final BitSet FOLLOW_term_in_touple293 = new BitSet(new long[]{0x0000000000090000L});
    public static final BitSet FOLLOW_19_in_touple310 = new BitSet(new long[]{0x0000000000000002L});

}