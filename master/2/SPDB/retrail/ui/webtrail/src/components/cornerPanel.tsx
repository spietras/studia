import styles from "./cornerPanel.module.css"

const positionStyles = {
    'top-left': styles.topLeft,
    'top-right': styles.topRight,
    'bottom-left': styles.bottomLeft,
    'bottom-right': styles.bottomRight
}

export default function CornerPanel({children, position = 'top-left'}) {
    return <div
        className={`${styles.panel} ${positionStyles[position]}`}>
        {children}
    </div>
}
