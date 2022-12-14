import {createProxyMiddleware} from 'http-proxy-middleware'

const apiUrl = process.env.WEBTRAIL_RETRAPI_URL || 'http://localhost:8080'

export const config = {
    api: {
        bodyParser: false, externalResolver: true,
    },
}

export default createProxyMiddleware({
    target: apiUrl, pathRewrite: {"^/api": ""}, changeOrigin: true
})
