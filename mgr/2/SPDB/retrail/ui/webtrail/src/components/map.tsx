import * as React from 'react'
import MapGL, {Layer, Source} from 'react-map-gl'
import {getBorder} from "../lib/api";
import {parseBorder} from "../lib/parse";
import mapboxgl from "mapbox-gl";

const borderLayerStyle = {
    type: 'line' as 'line', paint: {
        'line-color': '#888', 'line-width': 3
    }
}

export default function Map({apiKey}) {
    const [border, setBorder] = React.useState(null)
    const [viewport, setViewport] = React.useState({
        longitude: undefined, latitude: undefined, zoom: 10
    })

    const centerViewportToBorder = (border) => {
        const bounds = new mapboxgl.LngLatBounds(border.geometry.coordinates[0], border.geometry.coordinates[0])
        for (const coord of border.geometry.coordinates) {
            bounds.extend(coord);
        }
        setViewport({
            ...viewport,
            longitude: bounds.getCenter().lng,
            latitude: bounds.getCenter().lat
        })
    }

    const loadBorder = async () => setBorder(parseBorder(await getBorder()))

    React.useEffect(() => {
        loadBorder()
    }, [])

    React.useEffect(() => {
        if (border) centerViewportToBorder(border)
    }, [border])

    return <MapGL
        {...viewport}
        width='100vw'
        height='100vh'
        mapStyle='mapbox://styles/mapbox/streets-v11'
        mapboxApiAccessToken={apiKey}
        onViewportChange={setViewport}
    >
        <Source id="border" type="geojson" data={border}>
            <Layer {...borderLayerStyle} />
        </Source>
    </MapGL>
}
