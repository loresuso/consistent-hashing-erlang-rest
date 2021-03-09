import configController.ConfigController;
import gui.Gui;
import restclient.RestClient;

public class Main {

    public static void main(String[] args) {
        String url = ConfigController.getUrlFromConfig();
        if(url == null) {
            return;
        }
        RestClient.init(url);
        Gui.launch(Gui.class, args);
    }
}
