package gui;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import restclient.RestClient;

public class Gui extends Application {

    @Override
    public void start(Stage stage) throws Exception {
        if (!RestClient.isInitialized()) {
            GuiController.showAlert("Error: client not initialized");
        }
        Parent root = FXMLLoader.load(getClass().getResource("/gui.fxml"));
        stage.setTitle("Distributed Key-Value DB - Client");
        stage.setScene(new Scene(root, 600, 425));
        stage.setResizable(false);
        stage.initStyle(StageStyle.UNDECORATED);
        stage.show();
    }

    @Override
    public void stop() throws Exception {
        RestClient.close();
        super.stop();
    }

}
