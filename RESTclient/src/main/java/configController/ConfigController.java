package configController;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

public class ConfigController {


    public static String getUrlFromConfig() {
        try {
            String url;
            // create object mapper instance
            ObjectMapper mapper = new ObjectMapper();
            String filename = "configuration.json";
            // convert JSON file to map
            Map<?, ?> map = mapper.readValue(Paths.get(filename).toFile(), Map.class);

            // print map entries
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                System.out.println(entry.getKey() + "=" + entry.getValue());
            }
            url = (String) map.get("url");
            return url;
        } catch (Exception ex) {
            ex.printStackTrace();
            return "http://cappenet2.ddns.net:8080/kvstorage-1.0/rest";
        }
    }

    public static void setNewUrl(java.lang.String newUrl) {
        try {
            // create a map
            Map<String, Object> map = new HashMap<>();
            map.put("url", newUrl);

            // create object mapper instance
            ObjectMapper mapper = new ObjectMapper();

            // convert map to JSON file
            String filename = "configuration.json";
            mapper.writeValue(Paths.get(filename).toFile(), map);

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
