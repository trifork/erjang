// $ANTLR 3.1.2 /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g 2009-11-03 13:44:09

  package org.erlang.jbeam;


import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class beam2 extends TreeParser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "V", "String", "S", "Int", "EAtom", "Float", "L", "'file'", "'module'", "'exports'", "'nil'", "'move'", "'call'", "'get_touple_element'", "'test'", "'f'", "'x'", "'y'", "'float'", "'integer'", "'atom'"
    };
    public static final int L=10;
    public static final int T__24=24;
    public static final int T__23=23;
    public static final int T__22=22;
    public static final int T__21=21;
    public static final int T__20=20;
    public static final int V=4;
    public static final int S=6;
    public static final int EOF=-1;
    public static final int Int=7;
    public static final int Float=9;
    public static final int T__19=19;
    public static final int T__16=16;
    public static final int T__15=15;
    public static final int T__18=18;
    public static final int T__17=17;
    public static final int T__12=12;
    public static final int T__11=11;
    public static final int T__14=14;
    public static final int T__13=13;
    public static final int Symbol=8;
    public static final int String=5;

    // delegates
    // delegators


        public beam2(TreeNodeStream input) {
            this(input, new RecognizerSharedState());
        }
        public beam2(TreeNodeStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return beam2.tokenNames; }
    public String getGrammarFileName() { return "/Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g"; }


      



    // $ANTLR start "beamFile"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:16:1: beamFile : ^( V 'file' String ) ^( V 'module' symbol ) ^( V 'exports' ^( S ( exportSpec )* ) ) ;
    public final void beamFile() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:16:9: ( ^( V 'file' String ) ^( V 'module' symbol ) ^( V 'exports' ^( S ( exportSpec )* ) ) )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:17:3: ^( V 'file' String ) ^( V 'module' symbol ) ^( V 'exports' ^( S ( exportSpec )* ) )
            {
            match(input,V,FOLLOW_V_in_beamFile45); 

            match(input, Token.DOWN, null); 
            match(input,11,FOLLOW_11_in_beamFile47); 
            match(input,String,FOLLOW_String_in_beamFile49); 

            match(input, Token.UP, null); 
            match(input,V,FOLLOW_V_in_beamFile55); 

            match(input, Token.DOWN, null); 
            match(input,12,FOLLOW_12_in_beamFile57); 
            pushFollow(FOLLOW_symbol_in_beamFile59);
            symbol();

            state._fsp--;


            match(input, Token.UP, null); 
            match(input,V,FOLLOW_V_in_beamFile65); 

            match(input, Token.DOWN, null); 
            match(input,13,FOLLOW_13_in_beamFile67); 
            match(input,S,FOLLOW_S_in_beamFile70); 

            if ( input.LA(1)==Token.DOWN ) {
                match(input, Token.DOWN, null); 
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:19:21: ( exportSpec )*
                loop1:
                do {
                    int alt1=2;
                    int LA1_0 = input.LA(1);

                    if ( (LA1_0==V) ) {
                        alt1=1;
                    }


                    switch (alt1) {
                	case 1 :
                	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:19:21: exportSpec
                	    {
                	    pushFollow(FOLLOW_exportSpec_in_beamFile72);
                	    exportSpec();

                	    state._fsp--;


                	    }
                	    break;

                	default :
                	    break loop1;
                    }
                } while (true);


                match(input, Token.UP, null); 
            }

            match(input, Token.UP, null); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "beamFile"


    // $ANTLR start "exportSpec"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:22:1: exportSpec : ^( V fun= symbol ary= Int lab= Int ) ;
    public final void exportSpec() throws RecognitionException {
        Object ary=null;
        Object lab=null;

        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:22:11: ( ^( V fun= symbol ary= Int lab= Int ) )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:23:3: ^( V fun= symbol ary= Int lab= Int )
            {
            match(input,V,FOLLOW_V_in_exportSpec90); 

            match(input, Token.DOWN, null); 
            pushFollow(FOLLOW_symbol_in_exportSpec94);
            symbol();

            state._fsp--;

            ary=(Object)match(input,Int,FOLLOW_Int_in_exportSpec98); 
            lab=(Object)match(input,Int,FOLLOW_Int_in_exportSpec102); 

            match(input, Token.UP, null); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "exportSpec"


    // $ANTLR start "symbol"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:26:1: symbol : ( EAtom | keyWord );
    public final void symbol() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:26:7: ( EAtom | keyWord )
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( (LA2_0==Symbol) ) {
                alt2=1;
            }
            else if ( ((LA2_0>=11 && LA2_0<=14)) ) {
                alt2=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }
            switch (alt2) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:26:9: EAtom
                    {
                    match(input,Symbol,FOLLOW_Symbol_in_symbol113); 

                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:27:5: keyWord
                    {
                    pushFollow(FOLLOW_keyWord_in_symbol119);
                    keyWord();

                    state._fsp--;


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
        return ;
    }
    // $ANTLR end "symbol"


    // $ANTLR start "keyWord"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:29:1: keyWord : ( 'file' | 'module' | 'exports' | 'nil' );
    public final void keyWord() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:29:8: ( 'file' | 'module' | 'exports' | 'nil' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:
            {
            if ( (input.LA(1)>=11 && input.LA(1)<=14) ) {
                input.consume();
                state.errorRecovery=false;
            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                throw mse;
            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "keyWord"


    // $ANTLR start "insn"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:36:1: insn : ( ^( V 'move' term register ) | ^( V 'call' Int ^( V mod= EAtom fun= EAtom ary= Int ) ) | ^( V 'get_touple_element' src= register Int dst= register ) | ^( V 'test' symbol fail= label ^( S term term ) ) );
    public final void insn() throws RecognitionException {
        Object mod=null;
        Object fun=null;
        Object ary=null;

        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:36:5: ( ^( V 'move' term register ) | ^( V 'call' Int ^( V mod= EAtom fun= EAtom ary= Int ) ) | ^( V 'get_touple_element' src= register Int dst= register ) | ^( V 'test' symbol fail= label ^( S term term ) ) )
            int alt3=4;
            int LA3_0 = input.LA(1);

            if ( (LA3_0==V) ) {
                int LA3_1 = input.LA(2);

                if ( (LA3_1==DOWN) ) {
                    switch ( input.LA(3) ) {
                    case 15:
                        {
                        alt3=1;
                        }
                        break;
                    case 16:
                        {
                        alt3=2;
                        }
                        break;
                    case 17:
                        {
                        alt3=3;
                        }
                        break;
                    case 18:
                        {
                        alt3=4;
                        }
                        break;
                    default:
                        NoViableAltException nvae =
                            new NoViableAltException("", 3, 2, input);

                        throw nvae;
                    }

                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 3, 1, input);

                    throw nvae;
                }
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 3, 0, input);

                throw nvae;
            }
            switch (alt3) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:37:5: ^( V 'move' term register )
                    {
                    match(input,V,FOLLOW_V_in_insn167); 

                    match(input, Token.DOWN, null); 
                    match(input,15,FOLLOW_15_in_insn169); 
                    pushFollow(FOLLOW_term_in_insn171);
                    term();

                    state._fsp--;

                    pushFollow(FOLLOW_register_in_insn173);
                    register();

                    state._fsp--;


                    match(input, Token.UP, null); 

                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:39:5: ^( V 'call' Int ^( V mod= EAtom fun= EAtom ary= Int ) )
                    {
                    match(input,V,FOLLOW_V_in_insn186); 

                    match(input, Token.DOWN, null); 
                    match(input,16,FOLLOW_16_in_insn188); 
                    match(input,Int,FOLLOW_Int_in_insn190); 
                    match(input,V,FOLLOW_V_in_insn193); 

                    match(input, Token.DOWN, null); 
                    mod=(Object)match(input,Symbol,FOLLOW_Symbol_in_insn197); 
                    fun=(Object)match(input,Symbol,FOLLOW_Symbol_in_insn201); 
                    ary=(Object)match(input,Int,FOLLOW_Int_in_insn205); 

                    match(input, Token.UP, null); 

                    match(input, Token.UP, null); 

                    }
                    break;
                case 3 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:41:5: ^( V 'get_touple_element' src= register Int dst= register )
                    {
                    match(input,V,FOLLOW_V_in_insn217); 

                    match(input, Token.DOWN, null); 
                    match(input,17,FOLLOW_17_in_insn219); 
                    pushFollow(FOLLOW_register_in_insn223);
                    register();

                    state._fsp--;

                    match(input,Int,FOLLOW_Int_in_insn225); 
                    pushFollow(FOLLOW_register_in_insn229);
                    register();

                    state._fsp--;


                    match(input, Token.UP, null); 

                    }
                    break;
                case 4 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:43:5: ^( V 'test' symbol fail= label ^( S term term ) )
                    {
                    match(input,V,FOLLOW_V_in_insn240); 

                    match(input, Token.DOWN, null); 
                    match(input,18,FOLLOW_18_in_insn242); 
                    pushFollow(FOLLOW_symbol_in_insn244);
                    symbol();

                    state._fsp--;

                    pushFollow(FOLLOW_label_in_insn248);
                    label();

                    state._fsp--;

                    match(input,S,FOLLOW_S_in_insn251); 

                    match(input, Token.DOWN, null); 
                    pushFollow(FOLLOW_term_in_insn253);
                    term();

                    state._fsp--;

                    pushFollow(FOLLOW_term_in_insn255);
                    term();

                    state._fsp--;


                    match(input, Token.UP, null); 

                    match(input, Token.UP, null); 

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
        return ;
    }
    // $ANTLR end "insn"


    // $ANTLR start "label"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:47:1: label : ^( V 'f' Int ) ;
    public final void label() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:47:6: ( ^( V 'f' Int ) )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:47:8: ^( V 'f' Int )
            {
            match(input,V,FOLLOW_V_in_label271); 

            match(input, Token.DOWN, null); 
            match(input,19,FOLLOW_19_in_label273); 
            match(input,Int,FOLLOW_Int_in_label275); 

            match(input, Token.UP, null); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "label"


    // $ANTLR start "register"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:49:1: register : ( ^( V 'x' Int ) | ^( V 'y' Int ) );
    public final void register() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:49:9: ( ^( V 'x' Int ) | ^( V 'y' Int ) )
            int alt4=2;
            int LA4_0 = input.LA(1);

            if ( (LA4_0==V) ) {
                int LA4_1 = input.LA(2);

                if ( (LA4_1==DOWN) ) {
                    int LA4_2 = input.LA(3);

                    if ( (LA4_2==20) ) {
                        alt4=1;
                    }
                    else if ( (LA4_2==21) ) {
                        alt4=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 4, 2, input);

                        throw nvae;
                    }
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 4, 1, input);

                    throw nvae;
                }
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 4, 0, input);

                throw nvae;
            }
            switch (alt4) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:50:5: ^( V 'x' Int )
                    {
                    match(input,V,FOLLOW_V_in_register292); 

                    match(input, Token.DOWN, null); 
                    match(input,20,FOLLOW_20_in_register294); 
                    match(input,Int,FOLLOW_Int_in_register296); 

                    match(input, Token.UP, null); 

                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:51:5: ^( V 'y' Int )
                    {
                    match(input,V,FOLLOW_V_in_register304); 

                    match(input, Token.DOWN, null); 
                    match(input,21,FOLLOW_21_in_register306); 
                    match(input,Int,FOLLOW_Int_in_register308); 

                    match(input, Token.UP, null); 

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
        return ;
    }
    // $ANTLR end "register"


    // $ANTLR start "term"
    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:54:1: term : ( ^( V 'float' Float ) | ^( V 'integer' Int ) | ^( V 'atom' symbol ) | ^( V ( term )* ) | ^( L ( term )* ) | 'nil' | register );
    public final void term() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:54:5: ( ^( V 'float' Float ) | ^( V 'integer' Int ) | ^( V 'atom' symbol ) | ^( V ( term )* ) | ^( L ( term )* ) | 'nil' | register )
            int alt7=7;
            alt7 = dfa7.predict(input);
            switch (alt7) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:55:5: ^( V 'float' Float )
                    {
                    match(input,V,FOLLOW_V_in_term324); 

                    match(input, Token.DOWN, null); 
                    match(input,22,FOLLOW_22_in_term326); 
                    match(input,Float,FOLLOW_Float_in_term328); 

                    match(input, Token.UP, null); 

                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:56:5: ^( V 'integer' Int )
                    {
                    match(input,V,FOLLOW_V_in_term336); 

                    match(input, Token.DOWN, null); 
                    match(input,23,FOLLOW_23_in_term338); 
                    match(input,Int,FOLLOW_Int_in_term340); 

                    match(input, Token.UP, null); 

                    }
                    break;
                case 3 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:57:5: ^( V 'atom' symbol )
                    {
                    match(input,V,FOLLOW_V_in_term348); 

                    match(input, Token.DOWN, null); 
                    match(input,24,FOLLOW_24_in_term350); 
                    pushFollow(FOLLOW_symbol_in_term352);
                    symbol();

                    state._fsp--;


                    match(input, Token.UP, null); 

                    }
                    break;
                case 4 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:58:5: ^( V ( term )* )
                    {
                    match(input,V,FOLLOW_V_in_term360); 

                    if ( input.LA(1)==Token.DOWN ) {
                        match(input, Token.DOWN, null); 
                        // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:58:9: ( term )*
                        loop5:
                        do {
                            int alt5=2;
                            int LA5_0 = input.LA(1);

                            if ( (LA5_0==V||LA5_0==L||LA5_0==14) ) {
                                alt5=1;
                            }


                            switch (alt5) {
                        	case 1 :
                        	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:58:9: term
                        	    {
                        	    pushFollow(FOLLOW_term_in_term362);
                        	    term();

                        	    state._fsp--;


                        	    }
                        	    break;

                        	default :
                        	    break loop5;
                            }
                        } while (true);


                        match(input, Token.UP, null); 
                    }

                    }
                    break;
                case 5 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:59:5: ^( L ( term )* )
                    {
                    match(input,L,FOLLOW_L_in_term371); 

                    if ( input.LA(1)==Token.DOWN ) {
                        match(input, Token.DOWN, null); 
                        // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:59:9: ( term )*
                        loop6:
                        do {
                            int alt6=2;
                            int LA6_0 = input.LA(1);

                            if ( (LA6_0==V||LA6_0==L||LA6_0==14) ) {
                                alt6=1;
                            }


                            switch (alt6) {
                        	case 1 :
                        	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:59:9: term
                        	    {
                        	    pushFollow(FOLLOW_term_in_term373);
                        	    term();

                        	    state._fsp--;


                        	    }
                        	    break;

                        	default :
                        	    break loop6;
                            }
                        } while (true);


                        match(input, Token.UP, null); 
                    }

                    }
                    break;
                case 6 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:60:5: 'nil'
                    {
                    match(input,14,FOLLOW_14_in_term381); 

                    }
                    break;
                case 7 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam2.g:61:5: register
                    {
                    pushFollow(FOLLOW_register_in_term387);
                    register();

                    state._fsp--;


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
        return ;
    }
    // $ANTLR end "term"

    // Delegated rules


    protected DFA7 dfa7 = new DFA7(this);
    static final String DFA7_eotS =
        "\12\uffff";
    static final String DFA7_eofS =
        "\12\uffff";
    static final String DFA7_minS =
        "\1\4\1\2\2\uffff\1\3\5\uffff";
    static final String DFA7_maxS =
        "\1\16\1\2\2\uffff\1\30\5\uffff";
    static final String DFA7_acceptS =
        "\2\uffff\1\5\1\6\1\uffff\1\1\1\2\1\3\1\7\1\4";
    static final String DFA7_specialS =
        "\12\uffff}>";
    static final String[] DFA7_transitionS = {
            "\1\1\5\uffff\1\2\3\uffff\1\3",
            "\1\4",
            "",
            "",
            "\2\11\5\uffff\1\11\3\uffff\1\11\5\uffff\2\10\1\5\1\6\1\7",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA7_eot = DFA.unpackEncodedString(DFA7_eotS);
    static final short[] DFA7_eof = DFA.unpackEncodedString(DFA7_eofS);
    static final char[] DFA7_min = DFA.unpackEncodedStringToUnsignedChars(DFA7_minS);
    static final char[] DFA7_max = DFA.unpackEncodedStringToUnsignedChars(DFA7_maxS);
    static final short[] DFA7_accept = DFA.unpackEncodedString(DFA7_acceptS);
    static final short[] DFA7_special = DFA.unpackEncodedString(DFA7_specialS);
    static final short[][] DFA7_transition;

    static {
        int numStates = DFA7_transitionS.length;
        DFA7_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA7_transition[i] = DFA.unpackEncodedString(DFA7_transitionS[i]);
        }
    }

    class DFA7 extends DFA {

        public DFA7(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 7;
            this.eot = DFA7_eot;
            this.eof = DFA7_eof;
            this.min = DFA7_min;
            this.max = DFA7_max;
            this.accept = DFA7_accept;
            this.special = DFA7_special;
            this.transition = DFA7_transition;
        }
        public String getDescription() {
            return "54:1: term : ( ^( V 'float' Float ) | ^( V 'integer' Int ) | ^( V 'atom' symbol ) | ^( V ( term )* ) | ^( L ( term )* ) | 'nil' | register );";
        }
    }
 

    public static final BitSet FOLLOW_V_in_beamFile45 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_11_in_beamFile47 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_String_in_beamFile49 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_beamFile55 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_12_in_beamFile57 = new BitSet(new long[]{0x0000000000007900L});
    public static final BitSet FOLLOW_symbol_in_beamFile59 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_beamFile65 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_13_in_beamFile67 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_S_in_beamFile70 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_exportSpec_in_beamFile72 = new BitSet(new long[]{0x0000000000000018L});
    public static final BitSet FOLLOW_V_in_exportSpec90 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_symbol_in_exportSpec94 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_exportSpec98 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_exportSpec102 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_Symbol_in_symbol113 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_keyWord_in_symbol119 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_set_in_keyWord0 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_V_in_insn167 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_15_in_insn169 = new BitSet(new long[]{0x0000000000004410L});
    public static final BitSet FOLLOW_term_in_insn171 = new BitSet(new long[]{0x0000000000004410L});
    public static final BitSet FOLLOW_register_in_insn173 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_insn186 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_16_in_insn188 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_insn190 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_V_in_insn193 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_Symbol_in_insn197 = new BitSet(new long[]{0x0000000000000100L});
    public static final BitSet FOLLOW_Symbol_in_insn201 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_insn205 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_insn217 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_17_in_insn219 = new BitSet(new long[]{0x0000000000004410L});
    public static final BitSet FOLLOW_register_in_insn223 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_insn225 = new BitSet(new long[]{0x0000000000004410L});
    public static final BitSet FOLLOW_register_in_insn229 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_insn240 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_18_in_insn242 = new BitSet(new long[]{0x0000000000007900L});
    public static final BitSet FOLLOW_symbol_in_insn244 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_label_in_insn248 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_S_in_insn251 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_term_in_insn253 = new BitSet(new long[]{0x0000000000004410L});
    public static final BitSet FOLLOW_term_in_insn255 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_label271 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_19_in_label273 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_label275 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_register292 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_20_in_register294 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_register296 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_register304 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_21_in_register306 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_register308 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_term324 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_22_in_term326 = new BitSet(new long[]{0x0000000000000200L});
    public static final BitSet FOLLOW_Float_in_term328 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_term336 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_23_in_term338 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_Int_in_term340 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_term348 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_24_in_term350 = new BitSet(new long[]{0x0000000000007900L});
    public static final BitSet FOLLOW_symbol_in_term352 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_V_in_term360 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_term_in_term362 = new BitSet(new long[]{0x0000000000004418L});
    public static final BitSet FOLLOW_L_in_term371 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_term_in_term373 = new BitSet(new long[]{0x0000000000004418L});
    public static final BitSet FOLLOW_14_in_term381 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_register_in_term387 = new BitSet(new long[]{0x0000000000000002L});

}