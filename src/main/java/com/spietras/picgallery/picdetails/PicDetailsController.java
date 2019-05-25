package com.spietras.picgallery.picdetails;

import com.spietras.picgallery.picdetails.models.PicDetailsModel;
import com.spietras.picgallery.search.SearchController;
import com.spietras.picgallery.utils.ExecutorHelper;
import com.spietras.picgallery.utils.FXHelper;
import com.spietras.picgallery.utils.ZoomHelper;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.geometry.Point2D;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.ScrollEvent;
import javafx.scene.layout.StackPane;
import javafx.scene.text.Text;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class PicDetailsController
{
    private final SearchController searchController;
    private final PicDetailsModel model;
    private final ExecutorService downloadImageSingleThreadExecutor = Executors.newSingleThreadExecutor();
    @FXML
    private Button backButton;
    @FXML
    private ScrollPane scrollPane;
    @FXML
    private StackPane stackPane;
    @FXML
    private ImageView fullImageView;

    private double originalWidth;

    public PicDetailsController(SearchController searchController, PicDetailsModel model)
    {
        this.searchController = searchController;
        this.model = model;
    }

    @FXML
    public void initialize()
    {
        downloadAndSetPicture();
    }

    private void downloadAndSetPicture()
    {
        downloadImageSingleThreadExecutor.submit(() -> {
            try
            {
                Image fullImage = model.getFullImage();
                Platform.runLater(() -> setupImageView(fullImage));
            }
            catch(IOException e)
            {
                Platform.runLater(() -> handleImageLoadingException(e));
            }
        });
    }

    private void setupImageView(Image image)
    {
        fullImageView.setFitHeight(image.getHeight());
        fullImageView.setFitWidth(image.getWidth());
        fullImageView.setImage(image);

        originalWidth = image.getWidth();
        stackPane.setOnScroll(this::onScroll);
    }

    private void handleImageLoadingException(IOException e)
    {
        fullImageView.setDisable(true); //disable imageview so we can display text instead
        Text loadText = new Text("Couldn't load picture.\nClick here to load again");
        stackPane.getChildren().add(loadText);
        stackPane.setOnMouseClicked(event -> loadAgain(loadText));
        FXHelper.showErrorNotification("Error", "Couldn't load picture.\n" + e.getMessage(), 2000);
    }

    private void loadAgain(Node loadNode)
    {
        stackPane.getChildren().remove(loadNode);
        fullImageView.setDisable(false);
        downloadAndSetPicture();
    }

    public void shutdown(long timeout) throws IllegalThreadStateException
    {
        ExecutorHelper.abortExecutor(downloadImageSingleThreadExecutor, timeout);
    }

    @FXML
    void goBack(ActionEvent event)
    {
        try
        {
            searchController.resumeScene();
        }
        catch(IllegalStateException e)
        {
            FXHelper.showErrorNotification("Error", "Can't go back.\n" + e.getMessage(), 2000);
        }
    }

    private void onScroll(ScrollEvent e)
    {
        if(e.isControlDown()) //zoom with ctrl pressed
        {
            final double MIN_ZOOM = 0.2, MAX_ZOOM = 5.0;
            final double scaleFactor = 1.2;

            double zoomFactor = e.getDeltaY() > 0 ? scaleFactor : 1 / scaleFactor;

            ZoomHelper.zoomImageViewInsideScrollPane(fullImageView, new Point2D(e.getX(), e.getY()), scrollPane,
                                                     zoomFactor, MIN_ZOOM, MAX_ZOOM, originalWidth);

            e.consume(); //consume event so that scrollpane can't treat it as scroll
        }
    }
}
