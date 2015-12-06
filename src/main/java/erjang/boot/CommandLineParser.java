package erjang.boot;

import erjang.ErjangConfig;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/** Processing of VM parameter list.
 * The steps in this are as follows:
 * 1. Coalesce argument sources:
 *    - ERL_AFLAGS  (TODO)
 *    - ERL_FLAGS   (TODO)
 *    - ERL_ZFLAGS  (TODO)
 *    - Command line arguments
 * 2. Handle -args_list and -extra, i.e. argument file inclusion and partitioning
 *    into regular and extra/plain arguments.
 * 3. Handle special arguments (especially kernel flags)
 */
public class CommandLineParser {

    public static List<String> parseArgumentList(String[] args, ArrayList<String> base_args, ArrayList<String> extra_args) throws IOException {
        // Step 1: Handle -args_file inclusion and extra/regular partitioning.
        processArgsInto(args, base_args, extra_args);

        // Step 2: Handle and strip out any special arguments.
        handleSpecialArguments(base_args);

        if (ErjangConfig.hasString("erjang.boot.show_args")) {
            showArguments(base_args, extra_args);
        }
        List<String> res = new ArrayList<>(base_args);
        if (!extra_args.isEmpty()) {
            res.add("-extra");
            res.addAll(extra_args);
        }
        return res;
    }

    /** Process argument list, handling "-args_file" includes and partitioning into regular and extra arguments. */
    private static void processArgsInto(String[] args, List<String> base_args, List<String> extra_args) throws IOException {
        List<String> dest = base_args;
        for (int i=0; i<args.length; i++) {
            String arg = args[i];
            if (arg.equals("-args_file")) {
                i++;
                if (i == args.length) throw new RuntimeException("Bad command line: -args_file at end of line");
                parseArgsFile(args[i], base_args, extra_args);
            } else if ("-extra".equals(arg)) {
                // Change to extra_args for the rest of the command line
                dest = extra_args;
            } else {
                dest.add(arg);
            }
        }
    }

    private static void handleSpecialArguments(ArrayList<String> args) {
        for (int i=0; i<args.size(); i++) {
            String arg = args.get(i);
            if (arg.equals("-noinput") || arg.equals("-noshell")) {
                //special handling for -noshell / -noinput:
                //in this case we suppress the Progress wheel since it might break the output
                System.setProperty("erjang.progress.suppress", "true");
                // Keep.
            } else if (arg.startsWith("+")) {
                switch (arg.charAt(1)) {
                    case 'a':
                    case 'e': // strip erts version too
                    case 'A':
                    case 'B':
                    case 'K':
                    case 'M':
                    case 'P':
                    case 'R':
                    case 'S':
                    case 's':
                    case 't':
                    case 'T':
                    case 'W':
                        char flag = arg.charAt(1);
                        final String value;
                        if (arg.length() >= 2) {
                            value = arg.substring(2);
                            args.remove(i);
                        } else {
                            value = args.get(i+1);
                            args.remove(i);
                            args.remove(i);
                        }
                        System.setProperty("erjang.beam.option."+flag, value);
                        i--;
                        break;
                    case 'h': // And others?
                    default:
                        System.setProperty("erjang.beam.option."+arg.substring(1), "true");
                        args.remove(i);
                        i--;
                        break;
                }
            }
        }
    }

    private static void parseArgsFile(String filename, List<String> args, List<String> extra_args) throws IOException {
        try (BufferedReader file = new BufferedReader(new FileReader(filename))) {
            String line;
            List<String> tokens = new ArrayList();
            while ((line = file.readLine()) != null) {
                tokenizeInto(line, tokens);
            }

            List<String> dest = args;
            for (String token : tokens) {
                if ("-extra".equals(token)) {
                    // Change to extra_args for the rest of the file
                    dest = extra_args;
                } else {
                    dest.add(token);
                }
            }
        }
    }

    private static void tokenizeInto(String line, List<String> dest) throws IOException {
        // Slow but simple
        StringBuilder sb = new StringBuilder();
        for (int i=0; i<line.length(); i++) {
            char c = line.charAt(i);
            if (c=='\\') { // Handle character quoting
                i++;
                if (i==line.length()) throw new IOException("Syntax error: '\\' at end of line");
                sb.append(line.charAt(i));
            } else if (Character.isWhitespace(c)) { // End of token
                if (sb.length()>0) dest.add(sb.toString());
                sb.setLength(0);
            } else if (c=='#') { // Rest of line is comment
                break;
            }else { // Normal character
                sb.append(c);
            }
        }
        if (sb.length()>0) dest.add(sb.toString());
    }

    private static void showArguments(List<String> base_args, List<String> extra_args) {
        System.err.println("*** VM arguments:");
        System.err.println("    * Regular arguments:");
        for (String arg : base_args) {
            System.err.println("      "+arg);
        }
        System.err.println("    * Extra arguments:");
        for (String arg : extra_args) {
            System.err.println("      " + arg);
        }
        System.err.println();
    }
}
