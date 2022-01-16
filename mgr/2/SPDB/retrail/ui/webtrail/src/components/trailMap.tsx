import * as React from 'react'
import mapboxgl from "mapbox-gl"
import Line from "./line"
import Marker from "./marker"
import Map from "./map"
import {parseLines} from "../lib/parse"

const colors = {
    border: '#888', path: '#f00'
}

export default function TrailMap(props) {
    const {
        border,
        points,
        path,
        viewport,
        getPath,
        onPointsChange = async (_) => {
        },
        onViewportChange = async (_) => {
        },
        ...rest
    } = props

    const changePoints = async (points) => await onPointsChange(points)
    const changeViewport = async (viewport) => await onViewportChange(viewport)

    const centerViewportToBorder = async (newBorder, oldViewport) => {
        const bounds = new mapboxgl.LngLatBounds()
        for (const line of newBorder) {
            bounds.extend([line.start.x, line.start.y]);
        }
        await changeViewport({
            ...oldViewport,
            longitude: bounds.getCenter().lng,
            latitude: bounds.getCenter().lat
        })
    }

    const handleMapClick = async (e) => await changePoints([...points, {
        x: e.lngLat[0], y: e.lngLat[1]
    }])

    const handleMarkerDrag = async (i, e) => {
        const newPoints = [...points]
        newPoints[i] = {x: e.lngLat[0], y: e.lngLat[1]}
        await changePoints(newPoints)
    }

    React.useMemo(() => {
        if (border.length) centerViewportToBorder(border, viewport).then()
    }, [border])

    const markers = points.map(({x, y}, i) => <Marker
        key={i}
        x={x}
        y={y}
        onDragEnd={(e) => handleMarkerDrag(i, e)}
    />)

    return <Map
        viewport={viewport}
        onViewportChange={changeViewport}
        onClick={handleMapClick}
        {...rest}
    >
        <Line
            id='border'
            line-color={colors.border}
            data={parseLines(border)}
        />
        {markers}
        <Line
            id='path'
            line-color={colors.path}
            data={parseLines(path)}
        />
    </Map>
}
