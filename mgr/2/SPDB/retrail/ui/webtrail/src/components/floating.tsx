import styles from './floating.module.css'

const positionStyles = {
    'top-left': styles.topLeft,
    'top-right': styles.topRight,
    'bottom-left': styles.bottomLeft,
    'bottom-right': styles.bottomRight
}

export default function Floating({children, position = 'top-left'}) {
    return <div
        className={styles.floatingPane}>
        <div
            className={`${styles.floatingContainer} ${positionStyles[position]}`}>
            {children}
        </div>
    </div>
}
