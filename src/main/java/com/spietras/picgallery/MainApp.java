package com.spietras.picgallery;

import com.spietras.picgallery.models.picdata.pixabayData.PixabayEndpointAPI;
import com.spietras.picgallery.models.picdata.pixabayData.PixabayPictureDataProvider;
import com.spietras.picgallery.search.SearchController;
import com.spietras.picgallery.search.models.SearchDataModel;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

public class MainApp extends Application
{

    public static void main(String[] args)
    {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) throws Exception
    {
        System.setProperty("http.agent", "Chrome");

        Retrofit r = buildRetrofit(getApiKey());
        PixabayPictureDataProvider provider = new PixabayPictureDataProvider(r.create(PixabayEndpointAPI.class));
        SearchDataModel model = new SearchDataModel(provider);

        FXMLLoader loader = new FXMLLoader(getClass().getResource("/views/search.fxml"));
        SearchController controller = new SearchController(model);
        loader.setController(controller);
        Parent root = loader.load();

        primaryStage.setTitle("Picture Gallery");
        primaryStage.setScene(new Scene(root, 1280, 720));
        primaryStage.setOnHidden(e -> controller.shutdown());
        primaryStage.show();
    }

    private String getApiKey()
    {
        String API_KEY_PATH = "PixabayAPI_KEY.txt";

        InputStream is = ClassLoader.getSystemResourceAsStream(API_KEY_PATH);
        if(is != null)
        {
            BufferedReader reader = new BufferedReader(new InputStreamReader(is));
            return reader.lines().collect(Collectors.joining(System.lineSeparator()));
        }
        return null;
    }

    private Retrofit buildRetrofit(String apiKey)
    {
        OkHttpClient.Builder httpClient = new OkHttpClient.Builder();
        httpClient.addInterceptor(chain -> {
            Request original = chain.request();

            HttpUrl url = original.url().newBuilder()
                                  .addQueryParameter("key", apiKey)
                                  .build();

            Request request = original.newBuilder()
                                      .url(url)
                                      .build();

            return chain.proceed(request);
        });

        OkHttpClient client = httpClient.build();

        String PIXABAY_BASE_URL = "https://pixabay.com";

        return new Retrofit.Builder()
                .baseUrl(PIXABAY_BASE_URL)
                .addConverterFactory(GsonConverterFactory.create())
                .client(client)
                .build();
    }
}