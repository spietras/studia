import styles from './floating.module.css'

export default function Floating({children}) {
    return <div className={styles.floating}>{children}</div>
}
