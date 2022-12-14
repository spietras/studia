import styles from './button.module.css'

export default function Button({children, onClick = null}) {
    return <button
        className={styles.button}
        role={"button"}
        onClick={onClick}>
        {children}
    </button>
}
