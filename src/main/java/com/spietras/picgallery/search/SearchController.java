package com.spietras.picgallery.search;

import com.spietras.picgallery.models.picdata.PictureData;
import com.spietras.picgallery.models.picdata.pixabayData.PixabayRateLimitException;
import com.spietras.picgallery.picdetails.PicDetailsController;
import com.spietras.picgallery.picdetails.models.PicDetailsModel;
import com.spietras.picgallery.search.models.PictureTile;
import com.spietras.picgallery.search.models.SearchDataModel;
import com.spietras.picgallery.utils.ExecutorManager;
import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Rectangle2D;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.TilePane;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SearchController
{
    private final Stage stage;
    private final SearchDataModel model;
    @FXML
    private Button searchButton;
    @FXML
    private TextField inputTextField;
    @FXML
    private TilePane previewTilePane;
    @FXML
    private ScrollPane scroll;
    private ExecutorService loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();

    private volatile String currentQuery = null;

    private PicDetailsController currentDetailsController = null;
    private Parent searchParent = null;

    public SearchController(Stage stage, SearchDataModel model)
    {
        this.stage = stage;
        this.model = model;
    }

    @FXML
    public void initialize()
    {
        model.getPictures().addListener(this::onPicturesChanged);
        model.endOfPictures().addListener(
                (observable1, oldValue1, newValue1) -> onPicturesEndedChanged(newValue1));
        scroll.setVvalue(scroll.getVmax());
        scroll.vvalueProperty().addListener(
                (observable, oldValue, newValue) -> scrollVerticalChanged(newValue));
        scroll.layoutBoundsProperty().addListener((observableValue, bounds, t1) -> areaResized());
    }

    public void fetchParent()
    {
        searchParent = stage.getScene().getRoot();
    }

    public void shutdown()
    {
        if(currentDetailsController != null)
            currentDetailsController.shutdown();
        model.shutdown();
        ExecutorManager.shutdownExecutor(loadChunkSingleThreadExecutor, 60);
    }

    @FXML
    void onSearchButtonClicked(ActionEvent event)
    {
        String inputText = inputTextField.getText();
        searchAction(inputText, false);
    }

    private void searchAction(String query, boolean unfilledOnly)
    {
        if(query == null || query.isEmpty())
            return;

        searchButton.setDisable(true); //to prevent spamming

        checkQueryChanged(query); //check if query changed

        loadChunkSingleThreadExecutor.submit(() -> loadPictures(query, unfilledOnly));
    }

    private synchronized void checkQueryChanged(String newQuery)
    {
        if(!newQuery.equalsIgnoreCase(currentQuery))
        {
            currentQuery = newQuery;
            ExecutorManager.abortExecutor(loadChunkSingleThreadExecutor); //abort everything related to previous query
            loadChunkSingleThreadExecutor = Executors.newSingleThreadExecutor();
            model.clearPictures(); //clear all previous pictures
        }
    }

    private void loadPictures(String query, boolean unfilledOnly)
    {
        int CHUNK_SIZE = 10;

        try
        {
            if(unfilledOnly && contentFilledViewport())
                return;

            model.loadPicturesChunk(query, CHUNK_SIZE);
            Platform.runLater(() -> {
                if(!model.endOfPictures().getValue())
                    searchAction(query, true);
            });
        }
        catch(PixabayRateLimitException e)
        {
            e.printStackTrace();
            //TODO: handle exceeding rate limit
        }
        catch(IOException e)
        {
            e.printStackTrace();
            //TODO: handle other exceptions
        }
        finally
        {
            searchButton.setDisable(false); //unlock button after loading
        }

    }

    private boolean contentFilledViewport()
    {
        double contentWidth = previewTilePane.getWidth();
        int tilesInRow = (int) (contentWidth / (previewTilePane.getTileWidth() + previewTilePane.getHgap()));
        int rows = (int) Math.ceil((double) previewTilePane.getChildren().size() / tilesInRow);
        double singleRowSize = previewTilePane.getTileHeight() + previewTilePane.getVgap();
        double contentHeight = rows * singleRowSize;

        return contentHeight > scroll.getHeight() + singleRowSize;
    }

    private void onPicturesChanged(ListChangeListener.Change<? extends PictureTile> change)
    {
        try
        {
            while(change.next())
            {
                if(change.wasAdded())
                {
                    for(PictureTile p : change.getAddedSubList())
                    { addNewTileToView(p); }
                }
                else if(change.wasRemoved())
                    for(PictureTile p : change.getRemoved())
                    { removeTileFromView(p); }
            }
        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
    }

    private void addNewTileToView(PictureTile p)
    {
        double tileHeight = 80.0;
        previewTilePane.getChildren().add(createTilePreview(p, tileHeight));
    }

    private ImageView createTilePreview(PictureTile p, double tileHeight)
    {
        ImageView preview = new ImageView(p.getPreviewImage());
        preview.setFitHeight(tileHeight);
        preview.setPreserveRatio(true);
        preview.setViewport(createTileViewport(preview.getImage()));
        preview.addEventHandler(MouseEvent.MOUSE_CLICKED,
                                event -> showDetails(p.getData()));
        return preview;
    }

    private Rectangle2D createTileViewport(Image previewImage)
    {
        double smallerDimension = Math.min(previewImage.getWidth(), previewImage.getHeight());
        double viewportStartX = (previewImage.getWidth() - smallerDimension) / 2;
        double viewportStartY = (previewImage.getHeight() - smallerDimension) / 2;
        return new Rectangle2D(viewportStartX, viewportStartY, smallerDimension, smallerDimension);
    }

    private void removeTileFromView(PictureTile p)
    {
        previewTilePane.getChildren().removeIf(x -> ((ImageView) x).getImage() == p.getPreviewImage());
    }

    private void onPicturesEndedChanged(Boolean newValue)
    {
        if(newValue)
        {
            //TODO
        }
    }

    private void scrollVerticalChanged(Number newValue)
    {
        double scrollThreshold = 0.75;

        if(newValue.doubleValue() >= scrollThreshold * scroll.getVmax()) //when scrolled to end, load new chunk
            searchAction(currentQuery, false);
    }

    private void areaResized()
    {
        Platform.runLater(() -> {
            if(contentFilledViewport())
                return;
            searchAction(currentQuery, true);
        });
    }

    private void showDetails(PictureData data)
    {
        PicDetailsModel model = new PicDetailsModel(data);

        FXMLLoader loader = new FXMLLoader(getClass().getResource("/views/picdetails.fxml"));
        PicDetailsController controller = new PicDetailsController(this, model);
        loader.setController(controller);
        Parent root;
        try
        {
            root = loader.load(); //load details scene
        }
        catch(IOException e)
        {
            e.printStackTrace();
            return;
        }

        stage.getScene().setRoot(root); //show details scene
        currentDetailsController = controller;
    }

    public void resumeScene() throws IllegalStateException
    {
        if(searchParent == null)
            throw new IllegalStateException("Can't resume scene when parent is not set");

        stage.getScene().setRoot(searchParent);
        currentDetailsController.shutdown(); //clean everything in previous scene
        currentDetailsController = null;
    }
}
