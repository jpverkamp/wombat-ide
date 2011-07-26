package gui;

import scheme.SISCScheme;
import scheme.Scheme;

import java.util.HashMap;
import java.util.Map;

/**
 * Store options.
 */
public class Options {
    static Map<String, String> data;

    /**
     * Load default options.
     */
    static {
        data = new HashMap<String, String>();
        loadOptions();
    }

    public static void loadOptions() {
        data.clear();

        System.out.println("Options loading."); // TODO: Debug

        Scheme s = new SISCScheme();
        s.doStringAndWait("(define options '())");
        s.doStringAndWait("(define (cfg key val) (set! options (cons (list key val) options)))");
        s.doFileAndWait("options.cfg");

        String optionList = s.doStringAndWait("options");
        optionList = optionList.substring(2, optionList.length() - 2);
        optionList = optionList.replace("\"", "");
        for (String chunk : optionList.split("\\)\\s+\\(")) {
            String[] parts = chunk.split("\\s+", 2);
            data.put(parts[0], parts[1]);
        }

        System.out.println("Options loaded."); // TODO: Debug

    }

    /**
     * Access values. Null if it doesn't exist.
     *
     * @param key The key to look up.
     * @return The value or null.
     */
    public static String get(String key) {
        return get(key, null);
    }

    /**
     * Access values with a default it doesn't exist.
     *
     * @param key The key to look up.
     * @param def The default value.
     * @return The value or default.
     */
    @SuppressWarnings("unchecked")
    public static String get(String key, String def) {
        if (data.containsKey(key))
            return data.get(key);
        else
            return def;
    }
}
