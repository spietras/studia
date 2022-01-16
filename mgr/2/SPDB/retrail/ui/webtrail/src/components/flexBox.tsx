export default function FlexBox(props) {
    const {children, ...style} = props
    return <div style={{display: 'flex', ...style}}>{children}</div>
}
