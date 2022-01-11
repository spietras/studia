import Head from 'next/head'
import styles from './layout.module.css'

export default function Layout({children,}: { children: React.ReactNode }) {
    return (<div className={styles.container}>
        <Head>
            <link rel="icon" href="/favicon.svg"/>
            <meta name="description" content="retrail web ui"/>
        </Head>
        <main>{children}</main>
    </div>)
}
