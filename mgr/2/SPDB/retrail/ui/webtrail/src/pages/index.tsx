import Head from 'next/head'
import Layout from '../components/layout'
import Map from '../components/map'

export default function Index({apiKey}) {
    return (<Layout>
        <Head>
            <title>{"webtrail"}</title>
        </Head>
        <Map apiKey={apiKey}/>
    </Layout>)
}

export async function getServerSideProps() {
    return {
        props: {
            apiKey: process.env.WEBTRAIL_MAPBOX_API_KEY || null
        }
    }
}
