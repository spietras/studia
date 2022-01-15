import Head from 'next/head'
import Layout from '../components/layout'
import TrailMap from '../components/trailMap'
import Button from "../components/button";
import Floating from "../components/floating";
import * as React from "react";
import {getBorder, postFind} from "../lib/api";

export default function Index({apiKey}) {
    const [border, setBorder] = React.useState([])
    const [points, setPoints] = React.useState([])
    const [path, setPath] = React.useState([])
    const [viewport, setViewport] = React.useState(undefined)

    const loadBorder = async () => setBorder((await getBorder()).border.lines)
    const getPath = async () => (await postFind({points: points})).path.lines

    const handleResetClick = () => setPoints([])

    React.useEffect(() => {
        loadBorder().then()
    }, [])

    return (<Layout>
        <Head>
            <title>{"webtrail"}</title>
        </Head>
        <Floating>
            <div style={{margin: 10}}>
                <Button onClick={handleResetClick}>Reset</Button>
            </div>
        </Floating>
        <TrailMap
            border={border}
            points={points}
            path={path}
            viewport={viewport}
            getPath={getPath}
            onPointsChange={setPoints}
            onPathChange={setPath}
            onViewportChange={setViewport}
            apiKey={apiKey}
        />
    </Layout>)
}

export async function getServerSideProps() {
    return {
        props: {
            apiKey: process.env.WEBTRAIL_MAPBOX_API_KEY || null
        }
    }
}
