import { useEffect } from 'react'
import { useLocation } from 'react-router-dom'
import { trackInsightsPageViewed } from './insights'
import { classifyInsightsRoute } from './routes'

export function RouteAnalytics() {
  const location = useLocation()

  useEffect(() => {
    const page = classifyInsightsRoute(location.pathname)
    if (page) trackInsightsPageViewed(page)
  }, [location.pathname])

  return null
}
