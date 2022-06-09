import {Marker as MapGLMarker} from "react-map-gl"

export default function Marker(props) {
    const {x, y, width = 30, height = 30, ...rest} = props

    const offset = {
        left: -0.5 * width, top: -height
    }

    const style = {
        width: `${width}px`, height: `${height}px`
    }

    return <MapGLMarker
        longitude={x}
        latitude={y}
        draggable={true}
        offsetLeft={offset.left}
        offsetTop={offset.top}
        {...rest}
    >
        <img
            src="/marker.svg"
            alt="Marker"
            style={style}
            draggable={false}
            onDragStart={() => false}
        />
    </MapGLMarker>
}
