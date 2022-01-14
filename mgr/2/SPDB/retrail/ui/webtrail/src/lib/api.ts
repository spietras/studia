export const getBorder = async () => {
    const response = await fetch('/api/border')
    return await response.json()
}
