package gui;

import configController.ConfigController;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIconView;
import javafx.animation.*;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.shape.*;
import javafx.stage.Stage;
import javafx.util.Duration;
import restclient.RestClient;
import javax.swing.*;
import java.io.*;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.ResourceBundle;

public class GuiController extends JPanel implements Initializable  {
    @FXML
    private Button insert_button, get_button, delete_button;
    @FXML
    private Polygon put_marker, get_marker, del_marker;
    @FXML
    private VBox vbox_put, vbox_get, vbox_del;
    @FXML
    private Button do_it_insert, do_it_get, do_it_delete;
    @FXML
    private TextField key_insert, key_get, key_delete, value_insert;
    @FXML
    private TextArea result_insert, result_get, result_delete;
    @FXML
    private FontAwesomeIconView xbutton;
    @FXML
    private Pane tray_pane;
    @FXML
    private FontAwesomeIconView button_upload_file, button_save_file;
    @FXML
    private FontAwesomeIconView show_menu, show_menu1;
    @FXML
    private Pane config_menu;
    @FXML
    private TextField url_textfield;
    @FXML
    private Button change_url_button;
    @FXML
    private Label label_config;

    private double marker_current_pos = 0;
    private String value_content_from_file;
    private boolean use_file = false;
    private double x, y;


    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        button_upload_file.setGlyphName(FontAwesomeIcon.UPLOAD.name());
        button_save_file.setGlyphName(FontAwesomeIcon.SAVE.name());
        button_save_file.setVisible(false);
        xbutton.setGlyphName((FontAwesomeIcon.CLOSE.name()));
        show_menu.setGlyphName((FontAwesomeIcon.BARS.name()));
        show_menu1.setGlyphName((FontAwesomeIcon.CLOSE.name()));


        insert_button.setOnAction(actionEvent -> {

            vbox_put.setVisible(true);
            vbox_get.setVisible(false);
            vbox_del.setVisible(false);

            TranslateTransition translateTransition =
                    new TranslateTransition(Duration.millis(100), put_marker);
            translateTransition.setFromY(marker_current_pos);
            translateTransition.setToY(0);
            translateTransition.setCycleCount(1);
            translateTransition.setAutoReverse(false);
            translateTransition.play();
            marker_current_pos = 0;

        });

        get_button.setOnAction(actionEvent -> {
            value_content_from_file = null;
            use_file = false;
            value_insert.setDisable(false);

            vbox_put.setVisible(false);
            vbox_get.setVisible(true);
            vbox_del.setVisible(false);

            TranslateTransition translateTransition =
                    new TranslateTransition(Duration.millis(100), put_marker);
            translateTransition.setFromY(marker_current_pos);
            translateTransition.setToY(100);
            translateTransition.setCycleCount(1);
            translateTransition.setAutoReverse(false);
            translateTransition.play();
            marker_current_pos = 100;

        });

        delete_button.setOnAction(actionEvent -> {
            value_content_from_file = null;
            use_file = false;
            value_insert.setDisable(false);

            vbox_put.setVisible(false);
            vbox_get.setVisible(false);
            vbox_del.setVisible(true);


            TranslateTransition translateTransition =
                    new TranslateTransition(Duration.millis(100), put_marker);
            translateTransition.setFromY(marker_current_pos);
            translateTransition.setToY(200);
            translateTransition.setCycleCount(1);
            translateTransition.setAutoReverse(false);
            translateTransition.play();
            marker_current_pos = 200;

        });

        do_it_insert.setOnAction(actionEvent -> {
            String key = key_insert.getText();
            String value = value_insert.getText();
            if(key.isEmpty()) {
                showAlert("Type a valid key");
                return;
            }
            if(use_file) {
                String result = RestClient.put(key, value_content_from_file);
                result_insert.setText(result);
                use_file = false;
                value_content_from_file = null;
                value_insert.setDisable(false);
                return;
            }
            if(value.isEmpty()){
                showAlert("Type a valid value or load a file");
                return;
            }
            String result = RestClient.put(key, value);
            result_insert.setText(result);
        });

        do_it_get.setOnAction(actionEvent -> {
            String key = key_get.getText();
            if(key.isEmpty()) {
                showAlert("Type a valid key");
                return;
            }
            String result = RestClient.getByKey(key);
            result_get.setText(result);
            try{
                byte[] bytes = Base64.getDecoder().decode(result);
                button_save_file.setDisable(false);
                button_save_file.setVisible(true);
            } catch (IllegalArgumentException iae) {
                button_save_file.setDisable(true);
                button_save_file.setVisible(false);
            }
        });
        do_it_delete.setOnAction(actionEvent -> {
            String key = key_delete.getText();
            if(key.isEmpty()) {
                showAlert("Type a valid key");
                return;
            }
            String result = RestClient.deleteByKey(key);
            result_delete.setText(result);
        });


        xbutton.setOnMouseClicked(actionEvent -> {
            xbutton.getScene().getWindow().hide();
        });

        button_upload_file.setOnMouseClicked(actionEvent -> {
            String fileContent = null;
            use_file = false;
            try {
                fileContent = getFileContent();
            } catch (IOException e) {
                e.printStackTrace();
                use_file = false;
                return;
            }
            if(fileContent != null) {
                value_content_from_file = fileContent;
                use_file = true;
                value_insert.setDisable(true);
            }
        });

        button_save_file.setOnMouseClicked(actionEvent -> {
            if(result_get.getText().equals("")) {
                return;
            }
            String path = null;
            try {
                path = getPath();
                if(path == null) {
                    return;
                }
                String res = result_get.getText();
                byte[] bytes = Base64.getDecoder().decode(res.getBytes());
                Files.write(Paths.get(path), bytes);
                result_get.clear();
            } catch (IOException e) {
                e.printStackTrace();
                showAlert(e.getMessage());

            }

        });

        tray_pane.setOnMousePressed(actionEvent -> {
            x = actionEvent.getSceneX();
            y = actionEvent.getSceneY();
        });

        tray_pane.setOnMouseDragged(actionEvent -> {
            Stage stage = (Stage) ((Node) actionEvent.getSource()).getScene().getWindow();
            stage.setX(actionEvent.getScreenX() - x);
            stage.setY(actionEvent.getScreenY() - y);
        });


        show_menu.setOnMouseClicked(actionEvent -> {
            config_menu.setVisible(true);
            Duration cycleDuration = Duration.millis(500);
            Timeline timeline = new Timeline(
                    new KeyFrame(cycleDuration,
                            new KeyValue(config_menu.prefHeightProperty(),400,Interpolator.EASE_BOTH))

            );
            timeline.play();
            timeline.setOnFinished(event->{
                /* insert code here if you need */
                url_textfield.setVisible(true);
                show_menu1.setVisible(true);
                change_url_button.setVisible(true);
                label_config.setVisible(true);
                url_textfield.setText(ConfigController.getUrlFromConfig());
            });


        });


        show_menu1.setOnMouseClicked(actionEvent -> {

            show_menu1.setVisible(false);
            url_textfield.setVisible(false);
            change_url_button.setVisible(false);
            label_config.setVisible(false);

            Duration cycleDuration = Duration.millis(500);
            Timeline timeline = new Timeline(
                    new KeyFrame(cycleDuration,
                            new KeyValue(config_menu.prefHeightProperty(),0,Interpolator.EASE_BOTH))

            );

            timeline.play();
            timeline.setOnFinished(event->{
                /* insert code here if you need */
                config_menu.setVisible(false);
            });

        });

        change_url_button.setOnAction(actionEvent -> {
            String newUrl = url_textfield.getText();
            if(newUrl.isEmpty()) {
                return;
            }
            ConfigController.setNewUrl(newUrl);
            RestClient.setNewUrl(newUrl);
        });


    }


    private String getPath() {
        JFileChooser fileChooser = new JFileChooser();
        int n = fileChooser.showSaveDialog(GuiController.this);
        /*
        JFileChooser.CANCEL_OPTION: abbiamo selezionato il tasto Annulla
        JFileChooser.APPROVE_OPTION: abbiamo selezionato il tasto Apri
        JFileCHooser.ERROR_OPTION: si è verificato un errore
        */
        if( n != JFileChooser.APPROVE_OPTION) {
            return null;
        }
        return fileChooser.getSelectedFile().getAbsolutePath();
    }

    private String getFileContent() throws IOException {
        JFileChooser fileChooser = new JFileChooser();
        int n = fileChooser.showOpenDialog(GuiController.this);
        /*
        JFileChooser.CANCEL_OPTION: abbiamo selezionato il tasto Annulla
        JFileChooser.APPROVE_OPTION: abbiamo selezionato il tasto Apri
        JFileCHooser.ERROR_OPTION: si è verificato un errore
        */
        if( n != JFileChooser.APPROVE_OPTION) {
            return null;
        }
        String path = fileChooser.getSelectedFile().getAbsolutePath();
        byte[] bytes = Files.readAllBytes(Paths.get(path));
        return new String(Base64.getEncoder().encode(bytes));
    }

    public static void showAlert(String msg) {
        Alert alert = new Alert(Alert.AlertType.ERROR);
        alert.setTitle("Error");
        alert.setContentText(msg);
        alert.showAndWait();
    }


}
