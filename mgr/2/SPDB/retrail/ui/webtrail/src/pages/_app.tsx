import 'normalize.css/normalize.css'
import 'mapbox-gl/dist/mapbox-gl.css'
import '../styles/globals.css'
import {AppProps} from 'next/app'

export default function App({Component, pageProps}: AppProps) {
    return <Component {...pageProps} />
}
