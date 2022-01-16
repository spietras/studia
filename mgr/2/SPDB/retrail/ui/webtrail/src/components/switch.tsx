import styles from './switch.module.css'

const positionStyles = {
    left: styles.sliderLeft, right: styles.sliderRight
}

export default function Switch(props) {
    const {position, left, right, onClick} = props

    return <div className={styles.container}>
        <div className={styles.sliderContainer} onClick={onClick}>
            <div className={`${styles.slider} ${positionStyles[position]}`}/>
        </div>
        <div className={styles.switchContainerWrapper}>
            <div className={styles.switchContainer}>
                <div className={styles.switchLabel}>{left}</div>
                <div className={styles.switchLabel}>{right}</div>
            </div>
        </div>
    </div>
}
