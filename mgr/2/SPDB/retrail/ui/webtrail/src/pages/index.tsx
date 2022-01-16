import * as React from "react"
import Layout from '../components/layout'
import TrailMap from '../components/trailMap'
import Button from "../components/button"
import Floating from "../components/floating"
import Margin from "../components/margin"
import CornerPanel from "../components/cornerPanel"
import Label from "../components/label"
import Switch from "../components/switch";
import FlexBox from "../components/flexBox"
import {getBorder, postFind} from "../lib/api"

const title = 'webtrail'

const controlsMargin = 16

const modes = {
    distance: {
        position: 'left', label: 'Distance', unit: 'km'
    }, time: {
        position: 'right', label: 'Time', unit: 'h'
    }
}

export default function Index({apiKey}) {
    const [border, setBorder] = React.useState([])
    const [points, setPoints] = React.useState([])
    const [path, setPath] = React.useState([])
    const [cost, setCost] = React.useState(undefined)
    const [viewport, setViewport] = React.useState(undefined)
    const [mode, setMode] = React.useState('distance')

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
    const handleSwitchClick = () => setMode(mode === 'distance' ? 'time' : 'distance')

    const costLabel = (mode, cost) => `${modes[mode].label}: ${cost.toFixed(2)} ${modes[mode].unit}`
    const switchLabel = (mode) => modes[mode].label
    const switchPosition = (mode) => modes[mode].position

    React.useEffect(() => {
        loadBorder().then()
    }, [])

    React.useMemo(() => {
        setPath([])
        getPath().then()
    }, [points, mode])

    return (<Layout title={title}>
        <Floating>
            <Margin margin={`${controlsMargin}px`}>
                <FlexBox flexDirection='row'>
                    <Button onClick={handleResetClick}>Reset</Button>
                    <Margin margin={`0 ${controlsMargin}px`}>
                        <Switch
                            position={switchPosition(mode)}
                            left={switchLabel('distance')}
                            right={switchLabel('time')}
                            onClick={handleSwitchClick}
                        />
                    </Margin>
                </FlexBox>
            </Margin>
        </Floating>
        {(cost || cost === 0) && <Floating position='top-right'>
            <CornerPanel position='top-right'>
                <Margin>
                    <Label>{costLabel(mode, cost)}</Label>
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
