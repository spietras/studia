import {Layer, Source} from "react-map-gl"

const defaultPaint = {
    'line-color': '#000', 'line-width': 3
}

const defaultLayout = {
    'line-cap': 'round', 'line-join': 'round', 'line-round-limit': 0.5
}

export default function Line(props) {
    const {id, data, paint = {}, layout = {}} = props
    return <Source id={id} type="geojson" data={data}>
        <Layer
            type='line'
            paint={Object.assign({}, defaultPaint, paint)}
            layout={Object.assign({}, defaultLayout, layout)}
        />
    </Source>
}
