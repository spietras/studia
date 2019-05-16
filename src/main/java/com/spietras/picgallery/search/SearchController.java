package com.spietras.picgallery.search;

import com.spietras.picgallery.search.models.SearchDataModel;
import com.spietras.picgallery.search.models.picdata.PictureData;
import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.layout.TilePane;

import java.io.IOException;

public class SearchController
{
    @FXML
    private Button searchButton;

    @FXML
    private TextField inputTextField;

    @FXML
    private TilePane previewTilePane;

    private SearchDataModel model;

    public SearchController(SearchDataModel m)
    {
        model = m;
    }

    @FXML
    public void initialize()
    {
        model.getPictures().addListener((ListChangeListener<PictureData>) change -> {
            while(change.next())
            {
                if(change.wasAdded())
                {
                    for(PictureData p : change.getAddedSubList())
                    {
                        ImageView preview = new ImageView(p.getPreviewURL());
                        preview.setFitHeight(80);
                        preview.setPreserveRatio(true);
                        Platform.runLater(() -> previewTilePane.getChildren().add(preview));
                    }
                }
                else if(change.wasRemoved())
                {
                    Platform.runLater(
                            () -> previewTilePane.getChildren().remove(change.getFrom(), change.getRemovedSize()));
                }
            }
        });

        model.endOfPictures().addListener((observable, oldValue, newValue) -> {
            if(newValue)
            {
                //TODO
            }
        });
    }

    @FXML
    void onSearchButtonClicked(ActionEvent event)
    {
        new Thread(() -> {
            try
            {
                model.loadPicturesChunk(inputTextField.getText());
            }
            catch(IOException e)
            {
                e.printStackTrace();
            }
        }).start();
    }
}
