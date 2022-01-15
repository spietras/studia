import * as React from 'react'
import MapGL from 'react-map-gl'
import {getBorder} from "../lib/api";
import {parseBorder} from "../lib/parse";
import mapboxgl from "mapbox-gl";
import Border from "./border";
import Marker from "./marker";


export default function Map({apiKey}) {
    const [borderLines, setBorderLines] = React.useState([])
    const [points, setPoints] = React.useState([{
        x: 20.8519173, y: 52.1997174
    }])
    const [viewport, setViewport] = React.useState({
        longitude: undefined, latitude: undefined, zoom: 10
    })

    const centerViewportToBorder = (borderLines) => {
        const bounds = new mapboxgl.LngLatBounds()
        for (const line of borderLines) {
            bounds.extend([line.start.x, line.start.y]);
        }
        setViewport({
            ...viewport,
            longitude: bounds.getCenter().lng,
            latitude: bounds.getCenter().lat
        })
    }

    const loadBorder = async () => setBorderLines((await getBorder()).border.lines)

    const handleClick = (e) => setPoints([...points, {
        x: e.lngLat[0], y: e.lngLat[1]
    }])

    React.useEffect(() => {
        loadBorder()
    }, [])

    React.useEffect(() => {
        if (borderLines.length) centerViewportToBorder(borderLines)
    }, [borderLines])

    const markers = React.useMemo(() => points.map(({x, y}, i) => <Marker
        key={i} x={x} y={y}/>), [points])


    return <MapGL
        {...viewport}
        width='100vw'
        height='100vh'
        mapStyle='mapbox://styles/mapbox/streets-v11'
        mapboxApiAccessToken={apiKey}
        onViewportChange={setViewport}
        onClick={handleClick}
    >
        <Border data={parseBorder(borderLines)}/>
        {markers}
    </MapGL>
}
