import Head from 'next/head'
import styles from './layout.module.css'

export default function Layout({children, title}) {
    return (<div className={styles.container}>
        <Head>
            <title>{title}</title>
            <link rel="icon" href="/favicon.svg"/>
            <meta name="description" content="retrail web ui"/>
        </Head>
        <main>{children}</main>
    </div>)
}
