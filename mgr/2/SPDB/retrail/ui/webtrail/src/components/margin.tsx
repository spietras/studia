const defaultMargin = 16

export default function Margin({children, margin = defaultMargin}) {
    return <div
        style={{margin: margin}}>
        {children}
    </div>
}
