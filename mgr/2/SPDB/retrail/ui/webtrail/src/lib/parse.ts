const GeoJSON = require('geojson')

export const parseBorder = (lines) => {
    const points = lines.length
        ? lines.map((line) => [line.start.x, line.start.y]).concat([[lines.at(-1).end.x, lines.at(-1).end.y]])
        : []
    return GeoJSON.parse({line: points}, {LineString: 'line'})
}
