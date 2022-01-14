const GeoJSON = require('geojson')

export const parseBorder = ({border}) => {
    const points = border.lines.map((line) => [line.start.x, line.start.y]).concat([[border.lines.at(-1).end.x, border.lines.at(-1).end.y]])
    return GeoJSON.parse({line: points}, {LineString: 'line'})
}
