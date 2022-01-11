import Head from 'next/head'
import Layout from '../components/layout'
import Button from '../components/button'

export default function Index() {
    return (<Layout>
        <Head>
            <title>{"webtrail"}</title>
        </Head>
        <Button>Click</Button>
    </Layout>)
}
