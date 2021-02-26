package com.spietras.picgallery.utils;

import javafx.geometry.Point2D;
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.scene.image.ImageView;

public class ZoomHelper
{
    /**
     * Zooms ImageView with given zoomFactor and adjust scroll position so that mouse points at the same position as before
     *
     * @param zoomTarget    ImageView to zoom
     * @param pivotInTarget Mouse position in ImageView
     * @param holdingScroll ScrollPane that holds ImageView
     * @param zoomFactor    Zooming factor (ImageView size will be multiplied by this)
     * @param MIN_ZOOM      zoomFactor can't be less than this
     * @param MAX_ZOOM      zoomFactor can't be more than this
     * @param originalWidth Original width of ImageView
     */
    public static void zoomImageViewInsideScrollPane(ImageView zoomTarget, Point2D pivotInTarget,
                                                     ScrollPane holdingScroll, double zoomFactor, double MIN_ZOOM,
                                                     double MAX_ZOOM, double originalWidth)
    {
        //restrict zoom factor
        zoomFactor = clampZoomFactor(zoomTarget.getFitWidth(), zoomFactor, MIN_ZOOM, MAX_ZOOM, originalWidth);

        //get scroll offset in pixels
        Point2D scrollOffset = getScrollPixelOffset(new Point2D(holdingScroll.getHvalue(), holdingScroll.getVvalue()),
                                                    new Point2D(zoomTarget.getFitWidth(), zoomTarget.getFitHeight()),
                                                    new Point2D(holdingScroll.getWidth(), holdingScroll.getHeight()));

        //calculate how much offset should change after zoom
        Point2D adjustment = getScrollAdjustmentAfterZoom(zoomTarget, pivotInTarget, zoomFactor);

        resizeImageView(zoomTarget, zoomFactor);

        // refresh ScrollPane scroll positions & content bounds
        holdingScroll.layout();

        adjustScrollPosition(zoomTarget, holdingScroll, scrollOffset, adjustment);
    }

    private static double clampZoomFactor(double currentWidth, double zoomFactor, double MIN_ZOOM, double MAX_ZOOM,
                                          double originalWidth)
    {
        if(zoomFactor * currentWidth / originalWidth <= MIN_ZOOM)
            return MIN_ZOOM * originalWidth / currentWidth;
        else if(zoomFactor * currentWidth / originalWidth >= MAX_ZOOM)
            return MAX_ZOOM * originalWidth / currentWidth;
        else
            return zoomFactor;
    }

    private static Point2D getScrollPixelOffset(Point2D scrollValue, Point2D contentSize, Point2D scrollSize)
    {
        double valX = scrollValue.getX() * (contentSize.getX() - scrollSize.getX());
        double valY = scrollValue.getY() * (contentSize.getY() - scrollSize.getY());

        return new Point2D(valX, valY);
    }

    private static Point2D getScrollAdjustmentAfterZoom(Node target, Point2D pivotInTarget, double zoomFactor)
    {
        return target.localToParent(pivotInTarget.multiply(zoomFactor - 1));
    }

    private static void resizeImageView(ImageView target, double zoomFactor)
    {
        target.setFitWidth(zoomFactor * target.getFitWidth());
        target.setFitHeight(zoomFactor * target.getFitHeight());
    }

    private static void adjustScrollPosition(ImageView content, ScrollPane holdingScroll, Point2D previousScrollOffset,
                                             Point2D adjustment)
    {
        holdingScroll.setHvalue(
                (previousScrollOffset.getX() + adjustment.getX()) / (content.getFitWidth() - holdingScroll.getWidth()));
        holdingScroll.setVvalue((previousScrollOffset.getY() + adjustment.getY()) /
                                (content.getFitHeight() - holdingScroll.getHeight()));
    }
}
