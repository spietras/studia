import MapGL from 'react-map-gl'

const defaultViewport = {
    longitude: undefined, latitude: undefined, zoom: 10
}
const defaultMapStyle = 'mapbox://styles/mapbox/streets-v11'

export default function Map(props) {
    const {
        children,
        apiKey,
        viewport = defaultViewport,
        mapStyle = defaultMapStyle,
        ...rest
    } = props

    return <MapGL
        {...viewport}
        width='100vw'
        height='100vh'
        mapStyle={mapStyle}
        mapboxApiAccessToken={apiKey}
        {...rest}
    >
        {children}
    </MapGL>
}
