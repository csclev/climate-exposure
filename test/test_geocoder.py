import pytest
import pandas as pd
from src.geocoder import Geocoder
from pandas.testing import assert_frame_equal
from unittest.mock import MagicMock

def test_load_cache(tmp_path):
    cache_path = tmp_path/'cache.csv'
    """it loads an empty cache"""
    geocoder = Geocoder(cache_path)
    cache = geocoder.load_cache()
    assert isinstance(cache, pd.DataFrame)
    """it saves to the cache path provided"""
    query = "MyQuery"
    query_result = pd.DataFrame([{'query': query, "latitude": 12, "longitude": 3}])
    geocoder.geo_cache = pd.concat([geocoder.geo_cache, query_result])
    geocoder.save_cache()
    """it loads from a saved cache"""
    new_coder = Geocoder(cache_path)
    new_coder.load_cache()
    print((new_coder.geo_cache.iloc[[0]]))
    print(query_result)
    assert_frame_equal(new_coder.geo_cache.loc[[0],['query','latitude', 'longitude']], query_result)


def test_get_location_data(tmp_path):
    cache_path = tmp_path/'cache.csv'
    query = "Chicago, IL"
    cached_row = {
        "query": query
    }
    pd.DataFrame([cached_row]).to_csv(cache_path, index=False)

    geocoder = Geocoder(cache_path)
    geocoder.geocode = MagicMock()

    result = geocoder.get_location_data(query)
    assert result.loc[0, 'query'] == query

