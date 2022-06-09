const defaultMargin = '16px'

export default function Margin({children, margin = defaultMargin}) {
    return <div
        style={{margin: margin}}>
        {children}
    </div>
}
