from pathlib import Path
import pandas as pd
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter

class Geocoder():
    
    API_PROJECT_NAME = "ucb_mids_climate_exposure"

    def __init__(self, cache_file, project=API_PROJECT_NAME):
        self.cache_path = Path(cache_file)
        self.geo_cache = self.load_cache()
        self.geocode_agent = Nominatim(user_agent=project)
        self.geocode_service = RateLimiter(self.geocode_agent.geocode, min_delay_seconds=1, max_retries=2)
    
    def geocode(self, query: str) -> dict:
        """Returns a dict of location data for any query string"""
        query = query.strip()
        mask = self.geo_cache['query'] == query
        if mask.any():
            return self.geo_cache.loc[mask].iloc[0].to_dict()
        location = self.geocode_service(query)
        if location:
            address_dict = location.address
            new_row = {
                "query": query,
                "latitude": location.latitude,
                "longitude": location.longitude,
                "address": address_dict
            }
        else:
            new_row = {col: (query if col == 'query' else None) for col in self.geo_cache.columns}
        self.geo_cache = pd.concat([self.geo_cache, pd.DataFrame([new_row])], ignore_index=True)
        self.geo_cache.to_csv(self.cache_path, index=False)
        return new_row
    
    def load_cache(self):
        if self.cache_path.exists() and self.cache_path.stat().st_size > 0:
            return pd.read_csv(self.cache_path).filter(regex="^(?!Unnamed)")
        return pd.DataFrame({
        'query': pd.Series(dtype='str'),
        'address': pd.Series(dtype='str'),
        'latitude': pd.Series(dtype='float64'),
        'longitude': pd.Series(dtype='float64')
    })            
    def save_cache(self):
        self.geo_cache.to_csv(self.cache_path)

        

