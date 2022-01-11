import styles from './button.module.css'

export default function Button({children}: { children: React.ReactNode }) {
    const handleClick = async () => {
        const response = await fetch('/api/about')
        const text = await response.text()
        console.log(text)
    }

    return (<button
        className={styles.button}
        role={"button"}
        onClick={handleClick}>
        {children}
    </button>)
}
