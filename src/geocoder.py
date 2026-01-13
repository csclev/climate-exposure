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
        mask = self.geo_cache['query'] == query
        if mask.any():
            return self.geo_cache.loc[mask].iloc[0].to_dict()
        # TODO complete the function for calling the geocode API
        location = self.geocode(query)
        if location:
            address_dict = location.address
            new_row = {
                "query": query,
                "latitude": location.latitude,
                "longitude": location.longitude,
                "address": address_dict, 
                "city": address_dict.get('city') or address_dict.get('town') or address_dict.get('village'),
                "state": address_dict.get('state'),
                "postcode": address_dict.get('postcode'),
                "country": location.address.get('country')
            }
        else:
            new_row = {col: (query if col == 'query' else None) for col in self.geo_cache.columns}
        self.geo_cache = pd.concat([self.geo_cache, pd.DataFrame([new_row])], ignore_index=True)
        self.geo_cache.to_csv(self.cache_path, index=False)
        return new_row
    
    def load_cache(self):
        if self.cache_path.exists():
            return pd.read_csv(self.cache_path).filter(regex="^(?!Unnamed)")
        return pd.DataFrame(columns=['query', 'address', 'latitude', 'longitude', "city", "state", "postcode", "country" ])
            
    def save_cache(self):
        self.geo_cache.to_csv(self.cache_path)

        

