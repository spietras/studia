import {Layer, Source} from "react-map-gl"

const defaultPaint = {
    'line-color': '#000', 'line-width': 3
}

export default function Line(props) {
    const {id, data, ...rest} = props
    return <Source id={id} type="geojson" data={data}>
        <Layer type='line' paint={Object.assign({}, defaultPaint, {...rest})}/>
    </Source>
}
