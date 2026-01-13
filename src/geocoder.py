from pathlib import Path
import pandas as pd
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter

class Geocoder():
    
    API_PROJECT_NAME = "ucb_mids_climate_exposure"

    def __init__(self, cache_file, project=API_PROJECT_NAME):
        self.cache_path = Path(cache_file)
        self.geo_cache = self.load_cache()
        self.geocode_service = Nominatim(user_agent=project)
        self.geocode = RateLimiter(self.geocode_service.geocode, min_delay_seconds=1, max_retries=2)
    
    def get_location_data(self, query: str) -> dict:
        """Returns a dict of location data for any query string"""
        query = query.strip()
        cached = self.geo_cache.loc[self.geo_cache['query'] == query]
        if cached.empty:
            geodata = self.geocode.geocode(query)
            self.geo_cache.loc[query, ] = geodata
            self.geo_cache.to_csv(self.cache_path)
        else:
            geodata = cached
        return geodata
    
    def load_cache(self):
        if self.cache_path.exists():
            return pd.read_csv(self.cache_path)
        return pd.DataFrame(columns=['query', 'address', 'latitude', 'longitude' ])
            
    def save_cache(self):
        self.geo_cache.to_csv(self.cache_path)

        

