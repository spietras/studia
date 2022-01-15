import {Layer, Source} from "react-map-gl"
import * as React from "react"

const borderLayerStyle = {
    type: 'line' as 'line', paint: {
        'line-color': '#888', 'line-width': 3
    }
}

export default function Border({data, id = "border"}) {
    return <Source id={id} type="geojson" data={data}>
        <Layer {...borderLayerStyle} />
    </Source>
}
