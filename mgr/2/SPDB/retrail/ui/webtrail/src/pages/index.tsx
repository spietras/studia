import * as React from "react"
import Layout from '../components/layout'
import TrailMap from '../components/trailMap'
import Button from "../components/button"
import Floating from "../components/floating"
import Margin from "../components/margin"
import CornerPanel from "../components/cornerPanel"
import Label from "../components/label"
import {getBorder, postFind} from "../lib/api"

const title = 'webtrail'

export default function Index({apiKey}) {
    const [border, setBorder] = React.useState([])
    const [points, setPoints] = React.useState([])
    const [path, setPath] = React.useState([])
    const [cost, setCost] = React.useState(undefined)
    const [viewport, setViewport] = React.useState(undefined)

    const loadBorder = async () => setBorder((await getBorder()).border.lines)
    const getPath = async () => {
        if (points.length < 2) {
            setPath([])
            setCost(undefined)
        } else {
            const foundPath = await postFind({points: points})
            setPath(foundPath.path.lines)
            setCost(foundPath.cost)
        }
    }

    const handleResetClick = () => setPoints([])

    const costLabel = (cost) => `Distance: ${cost.toFixed(2)} km`

    React.useEffect(() => {
        loadBorder().then()
    }, [])

    React.useMemo(() => {
        getPath().then()
    }, [points])

    return (<Layout title={title}>
        <Floating>
            <Margin>
                <Button onClick={handleResetClick}>Reset</Button>
            </Margin>
        </Floating>
        {(cost || cost === 0) && <Floating position='top-right'>
            <CornerPanel position='top-right'>
                <Margin>
                    <Label>{costLabel(cost)}</Label>
                </Margin>
            </CornerPanel>
        </Floating>}
        <TrailMap
            border={border}
            points={points}
            path={path}
            viewport={viewport}
            getPath={getPath}
            onPointsChange={setPoints}
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
