import {Marker as MapGLMarker} from "react-map-gl"
import * as React from "react"
import styles from './marker.module.css'

export default function Marker({x, y}) {
    return <MapGLMarker longitude={x} latitude={y}>
        <img src="/favicon.svg" alt="Marker" className={styles.marker}/>
    </MapGLMarker>
}
