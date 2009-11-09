// $ANTLR 3.1.2 /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g 2009-11-05 21:55:22

   package org.erlang.jbeam; 
   import org.erlang.*;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class beamLexer extends Lexer {
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

    public beamLexer() {;} 
    public beamLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public beamLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "/Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g"; }

    // $ANTLR start "T__14"
    public final void mT__14() throws RecognitionException {
        try {
            int _type = T__14;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:12:7: ( '.' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:12:9: '.'
            {
            match('.'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__14"

    // $ANTLR start "T__15"
    public final void mT__15() throws RecognitionException {
        try {
            int _type = T__15;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:13:7: ( '[' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:13:9: '['
            {
            match('['); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__15"

    // $ANTLR start "T__16"
    public final void mT__16() throws RecognitionException {
        try {
            int _type = T__16;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:14:7: ( ',' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:14:9: ','
            {
            match(','); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__16"

    // $ANTLR start "T__17"
    public final void mT__17() throws RecognitionException {
        try {
            int _type = T__17;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:15:7: ( ']' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:15:9: ']'
            {
            match(']'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__17"

    // $ANTLR start "T__18"
    public final void mT__18() throws RecognitionException {
        try {
            int _type = T__18;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:16:7: ( '{' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:16:9: '{'
            {
            match('{'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__18"

    // $ANTLR start "T__19"
    public final void mT__19() throws RecognitionException {
        try {
            int _type = T__19;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:17:7: ( '}' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:17:9: '}'
            {
            match('}'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__19"

    // $ANTLR start "Atom"
    public final void mAtom() throws RecognitionException {
        try {
            int _type = Atom;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            Token body=null;

            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:81:5: ( ( 'a' .. 'z' ( 'a' .. 'z' | 'A' .. 'Z' | Digit | '@' | '_' )* ) | ( '\\'' body= AtomBody '\\'' ) )
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( ((LA2_0>='a' && LA2_0<='z')) ) {
                alt2=1;
            }
            else if ( (LA2_0=='\'') ) {
                alt2=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }
            switch (alt2) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:81:7: ( 'a' .. 'z' ( 'a' .. 'z' | 'A' .. 'Z' | Digit | '@' | '_' )* )
                    {
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:81:7: ( 'a' .. 'z' ( 'a' .. 'z' | 'A' .. 'Z' | Digit | '@' | '_' )* )
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:81:8: 'a' .. 'z' ( 'a' .. 'z' | 'A' .. 'Z' | Digit | '@' | '_' )*
                    {
                    matchRange('a','z'); 
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:81:17: ( 'a' .. 'z' | 'A' .. 'Z' | Digit | '@' | '_' )*
                    loop1:
                    do {
                        int alt1=2;
                        int LA1_0 = input.LA(1);

                        if ( ((LA1_0>='0' && LA1_0<='9')||(LA1_0>='@' && LA1_0<='Z')||LA1_0=='_'||(LA1_0>='a' && LA1_0<='z')) ) {
                            alt1=1;
                        }


                        switch (alt1) {
                    	case 1 :
                    	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:
                    	    {
                    	    if ( (input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='@' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                    	        input.consume();

                    	    }
                    	    else {
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;

                    	default :
                    	    break loop1;
                        }
                    } while (true);


                    }


                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:82:7: ( '\\'' body= AtomBody '\\'' )
                    {
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:82:7: ( '\\'' body= AtomBody '\\'' )
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:82:8: '\\'' body= AtomBody '\\''
                    {
                    match('\''); 
                    int bodyStart123 = getCharIndex();
                    mAtomBody(); 
                    body = new CommonToken(input, Token.INVALID_TOKEN_TYPE, Token.DEFAULT_CHANNEL, bodyStart123, getCharIndex()-1);
                    match('\''); 
                     state.text = (body!=null?body.getText():null); 

                    }


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "Atom"

    // $ANTLR start "AtomBody"
    public final void mAtomBody() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:85:18: ( ( AtomChar )* )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:85:20: ( AtomChar )*
            {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:85:20: ( AtomChar )*
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( ((LA3_0>='\u0000' && LA3_0<='&')||(LA3_0>='(' && LA3_0<='\uFFFF')) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:85:20: AtomChar
            	    {
            	    mAtomChar(); 

            	    }
            	    break;

            	default :
            	    break loop3;
                }
            } while (true);


            }

        }
        finally {
        }
    }
    // $ANTLR end "AtomBody"

    // $ANTLR start "Digit"
    public final void mDigit() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:87:15: ( '0' .. '9' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:87:17: '0' .. '9'
            {
            matchRange('0','9'); 

            }

        }
        finally {
        }
    }
    // $ANTLR end "Digit"

    // $ANTLR start "AtomChar"
    public final void mAtomChar() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:89:18: ( '\\\\' Digit Digit Digit | '\\\\' ~ Digit | ~ ( '\\\\' | '\\'' ) )
            int alt4=3;
            int LA4_0 = input.LA(1);

            if ( (LA4_0=='\\') ) {
                int LA4_1 = input.LA(2);

                if ( ((LA4_1>='\u0000' && LA4_1<='/')||(LA4_1>=':' && LA4_1<='\uFFFF')) ) {
                    alt4=2;
                }
                else if ( ((LA4_1>='0' && LA4_1<='9')) ) {
                    alt4=1;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 4, 1, input);

                    throw nvae;
                }
            }
            else if ( ((LA4_0>='\u0000' && LA4_0<='&')||(LA4_0>='(' && LA4_0<='[')||(LA4_0>=']' && LA4_0<='\uFFFF')) ) {
                alt4=3;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 4, 0, input);

                throw nvae;
            }
            switch (alt4) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:90:7: '\\\\' Digit Digit Digit
                    {
                    match('\\'); 
                    mDigit(); 
                    mDigit(); 
                    mDigit(); 

                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:91:7: '\\\\' ~ Digit
                    {
                    match('\\'); 
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u0007')||(input.LA(1)>='\t' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}


                    }
                    break;
                case 3 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:92:7: ~ ( '\\\\' | '\\'' )
                    {
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='&')||(input.LA(1)>='(' && input.LA(1)<='[')||(input.LA(1)>=']' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}


                    }
                    break;

            }
        }
        finally {
        }
    }
    // $ANTLR end "AtomChar"

    // $ANTLR start "Float"
    public final void mFloat() throws RecognitionException {
        try {
            int _type = Float;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:6: ( ( '-' )? ( Digit )+ '.' ( Digit )+ ( 'e' ( Digit )+ )? )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:8: ( '-' )? ( Digit )+ '.' ( Digit )+ ( 'e' ( Digit )+ )?
            {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:8: ( '-' )?
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( (LA5_0=='-') ) {
                alt5=1;
            }
            switch (alt5) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:8: '-'
                    {
                    match('-'); 

                    }
                    break;

            }

            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:13: ( Digit )+
            int cnt6=0;
            loop6:
            do {
                int alt6=2;
                int LA6_0 = input.LA(1);

                if ( ((LA6_0>='0' && LA6_0<='9')) ) {
                    alt6=1;
                }


                switch (alt6) {
            	case 1 :
            	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:13: Digit
            	    {
            	    mDigit(); 

            	    }
            	    break;

            	default :
            	    if ( cnt6 >= 1 ) break loop6;
                        EarlyExitException eee =
                            new EarlyExitException(6, input);
                        throw eee;
                }
                cnt6++;
            } while (true);

            match('.'); 
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:24: ( Digit )+
            int cnt7=0;
            loop7:
            do {
                int alt7=2;
                int LA7_0 = input.LA(1);

                if ( ((LA7_0>='0' && LA7_0<='9')) ) {
                    alt7=1;
                }


                switch (alt7) {
            	case 1 :
            	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:24: Digit
            	    {
            	    mDigit(); 

            	    }
            	    break;

            	default :
            	    if ( cnt7 >= 1 ) break loop7;
                        EarlyExitException eee =
                            new EarlyExitException(7, input);
                        throw eee;
                }
                cnt7++;
            } while (true);

            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:31: ( 'e' ( Digit )+ )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0=='e') ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:32: 'e' ( Digit )+
                    {
                    match('e'); 
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:36: ( Digit )+
                    int cnt8=0;
                    loop8:
                    do {
                        int alt8=2;
                        int LA8_0 = input.LA(1);

                        if ( ((LA8_0>='0' && LA8_0<='9')) ) {
                            alt8=1;
                        }


                        switch (alt8) {
                    	case 1 :
                    	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:95:36: Digit
                    	    {
                    	    mDigit(); 

                    	    }
                    	    break;

                    	default :
                    	    if ( cnt8 >= 1 ) break loop8;
                                EarlyExitException eee =
                                    new EarlyExitException(8, input);
                                throw eee;
                        }
                        cnt8++;
                    } while (true);


                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "Float"

    // $ANTLR start "Integer"
    public final void mInteger() throws RecognitionException {
        try {
            int _type = Integer;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:96:8: ( ( '-' )? ( Digit )+ )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:96:10: ( '-' )? ( Digit )+
            {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:96:10: ( '-' )?
            int alt10=2;
            int LA10_0 = input.LA(1);

            if ( (LA10_0=='-') ) {
                alt10=1;
            }
            switch (alt10) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:96:10: '-'
                    {
                    match('-'); 

                    }
                    break;

            }

            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:96:15: ( Digit )+
            int cnt11=0;
            loop11:
            do {
                int alt11=2;
                int LA11_0 = input.LA(1);

                if ( ((LA11_0>='0' && LA11_0<='9')) ) {
                    alt11=1;
                }


                switch (alt11) {
            	case 1 :
            	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:96:15: Digit
            	    {
            	    mDigit(); 

            	    }
            	    break;

            	default :
            	    if ( cnt11 >= 1 ) break loop11;
                        EarlyExitException eee =
                            new EarlyExitException(11, input);
                        throw eee;
                }
                cnt11++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "Integer"

    // $ANTLR start "String"
    public final void mString() throws RecognitionException {
        try {
            int _type = String;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            Token body=null;

            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:98:7: ( '\"' body= StringBody '\"' )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:98:9: '\"' body= StringBody '\"'
            {
            match('\"'); 
            int bodyStart262 = getCharIndex();
            mStringBody(); 
            body = new CommonToken(input, Token.INVALID_TOKEN_TYPE, Token.DEFAULT_CHANNEL, bodyStart262, getCharIndex()-1);
            match('\"'); 
             state.text = (body!=null?body.getText():null); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "String"

    // $ANTLR start "StringBody"
    public final void mStringBody() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:100:20: ( ( StringChar )* )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:100:22: ( StringChar )*
            {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:100:22: ( StringChar )*
            loop12:
            do {
                int alt12=2;
                int LA12_0 = input.LA(1);

                if ( ((LA12_0>='\u0000' && LA12_0<='!')||(LA12_0>='#' && LA12_0<='\uFFFF')) ) {
                    alt12=1;
                }


                switch (alt12) {
            	case 1 :
            	    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:100:22: StringChar
            	    {
            	    mStringChar(); 

            	    }
            	    break;

            	default :
            	    break loop12;
                }
            } while (true);


            }

        }
        finally {
        }
    }
    // $ANTLR end "StringBody"

    // $ANTLR start "StringChar"
    public final void mStringChar() throws RecognitionException {
        try {
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:102:20: ( '\\\\' Digit Digit Digit | '\\\\' ~ Digit | ~ ( '\\\\' | '\"' ) )
            int alt13=3;
            int LA13_0 = input.LA(1);

            if ( (LA13_0=='\\') ) {
                int LA13_1 = input.LA(2);

                if ( ((LA13_1>='\u0000' && LA13_1<='/')||(LA13_1>=':' && LA13_1<='\uFFFF')) ) {
                    alt13=2;
                }
                else if ( ((LA13_1>='0' && LA13_1<='9')) ) {
                    alt13=1;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 13, 1, input);

                    throw nvae;
                }
            }
            else if ( ((LA13_0>='\u0000' && LA13_0<='!')||(LA13_0>='#' && LA13_0<='[')||(LA13_0>=']' && LA13_0<='\uFFFF')) ) {
                alt13=3;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 13, 0, input);

                throw nvae;
            }
            switch (alt13) {
                case 1 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:103:6: '\\\\' Digit Digit Digit
                    {
                    match('\\'); 
                    mDigit(); 
                    mDigit(); 
                    mDigit(); 

                    }
                    break;
                case 2 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:104:6: '\\\\' ~ Digit
                    {
                    match('\\'); 
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u0007')||(input.LA(1)>='\t' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}


                    }
                    break;
                case 3 :
                    // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:105:6: ~ ( '\\\\' | '\"' )
                    {
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='!')||(input.LA(1)>='#' && input.LA(1)<='[')||(input.LA(1)>=']' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}


                    }
                    break;

            }
        }
        finally {
        }
    }
    // $ANTLR end "StringChar"

    // $ANTLR start "WS"
    public final void mWS() throws RecognitionException {
        try {
            int _type = WS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:107:3: ( ( ' ' | '\\t' | '\\n' ) )
            // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:107:5: ( ' ' | '\\t' | '\\n' )
            {
            if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||input.LA(1)==' ' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

             _channel = HIDDEN; 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WS"

    public void mTokens() throws RecognitionException {
        // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:8: ( T__14 | T__15 | T__16 | T__17 | T__18 | T__19 | Atom | Float | Integer | String | WS )
        int alt14=11;
        alt14 = dfa14.predict(input);
        switch (alt14) {
            case 1 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:10: T__14
                {
                mT__14(); 

                }
                break;
            case 2 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:16: T__15
                {
                mT__15(); 

                }
                break;
            case 3 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:22: T__16
                {
                mT__16(); 

                }
                break;
            case 4 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:28: T__17
                {
                mT__17(); 

                }
                break;
            case 5 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:34: T__18
                {
                mT__18(); 

                }
                break;
            case 6 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:40: T__19
                {
                mT__19(); 

                }
                break;
            case 7 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:46: Atom
                {
                mAtom(); 

                }
                break;
            case 8 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:51: Float
                {
                mFloat(); 

                }
                break;
            case 9 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:57: Integer
                {
                mInteger(); 

                }
                break;
            case 10 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:65: String
                {
                mString(); 

                }
                break;
            case 11 :
                // /Users/krab/Documents/humio/erlang/src/org/erlang/jbeam/beam.g:1:72: WS
                {
                mWS(); 

                }
                break;

        }

    }


    protected DFA14 dfa14 = new DFA14(this);
    static final String DFA14_eotS =
        "\11\uffff\1\14\4\uffff";
    static final String DFA14_eofS =
        "\16\uffff";
    static final String DFA14_minS =
        "\1\11\7\uffff\1\60\1\56\4\uffff";
    static final String DFA14_maxS =
        "\1\175\7\uffff\2\71\4\uffff";
    static final String DFA14_acceptS =
        "\1\uffff\1\1\1\2\1\3\1\4\1\5\1\6\1\7\2\uffff\1\12\1\13\1\11\1\10";
    static final String DFA14_specialS =
        "\16\uffff}>";
    static final String[] DFA14_transitionS = {
            "\2\13\25\uffff\1\13\1\uffff\1\12\4\uffff\1\7\4\uffff\1\3\1\10"+
            "\1\1\1\uffff\12\11\41\uffff\1\2\1\uffff\1\4\3\uffff\32\7\1\5"+
            "\1\uffff\1\6",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\12\11",
            "\1\15\1\uffff\12\11",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA14_eot = DFA.unpackEncodedString(DFA14_eotS);
    static final short[] DFA14_eof = DFA.unpackEncodedString(DFA14_eofS);
    static final char[] DFA14_min = DFA.unpackEncodedStringToUnsignedChars(DFA14_minS);
    static final char[] DFA14_max = DFA.unpackEncodedStringToUnsignedChars(DFA14_maxS);
    static final short[] DFA14_accept = DFA.unpackEncodedString(DFA14_acceptS);
    static final short[] DFA14_special = DFA.unpackEncodedString(DFA14_specialS);
    static final short[][] DFA14_transition;

    static {
        int numStates = DFA14_transitionS.length;
        DFA14_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA14_transition[i] = DFA.unpackEncodedString(DFA14_transitionS[i]);
        }
    }

    class DFA14 extends DFA {

        public DFA14(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 14;
            this.eot = DFA14_eot;
            this.eof = DFA14_eof;
            this.min = DFA14_min;
            this.max = DFA14_max;
            this.accept = DFA14_accept;
            this.special = DFA14_special;
            this.transition = DFA14_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( T__14 | T__15 | T__16 | T__17 | T__18 | T__19 | Atom | Float | Integer | String | WS );";
        }
    }
 

}