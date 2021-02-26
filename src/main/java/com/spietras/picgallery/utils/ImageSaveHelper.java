package com.spietras.picgallery.utils;

import javafx.embed.swing.SwingFXUtils;
import javafx.scene.image.Image;
import javafx.stage.FileChooser;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class ImageSaveHelper
{
    /**
     * Saves given Image to given File
     *
     * @param image      Image to save
     * @param file       File to save to
     * @param fileFormat Format of file
     * @throws IOException When can't save
     */
    public static void saveImage(Image image, File file, String fileFormat) throws IOException
    {
        BufferedImage bufferedImage = convertImage(image);
        ImageIO.write(bufferedImage, fileFormat, file);
    }

    private static BufferedImage convertImage(Image image)
    {
        return SwingFXUtils.fromFXImage(image, null); //use swing utils to convert to BufferedImage
    }

    /**
     * Displays dialog to choose file and returns chosen file
     *
     * @param fileFormat Default file format to display
     * @return Chosen file
     */
    public static File chooseFile(String fileFormat)
    {
        FileChooser chooser = new FileChooser();
        chooser.setTitle("Save file");
        chooser.setInitialFileName("." + fileFormat);
        return chooser.showSaveDialog(null);
    }
}
