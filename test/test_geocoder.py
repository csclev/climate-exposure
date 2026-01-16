import pytest
import pandas as pd
from src.geocoder import Geocoder
from pandas.testing import assert_frame_equal
from unittest.mock import MagicMock

@pytest.fixture
def geo(tmp_path):
    """Provide a Geocoder instance with a clean temp cache"""
    cache_path = tmp_path / "cache.csv"
    return Geocoder(cache_path)

def test_load_cache(geo):
    """it loads an empty cache"""
    cache = geo.load_cache()
    assert isinstance(cache, pd.DataFrame)
    assert cache['query'].any() == False
    """it saves to the cache path provided"""
    query = "MyQuery"
    query_result = pd.DataFrame([{'query': query, "latitude": 12, "longitude": 3}])
    geo.geo_cache = pd.concat([geo.geo_cache, query_result])
    geo.save_cache()
    """it loads from a saved cache"""
    new_coder = Geocoder(geo.cache_path)
    new_coder.load_cache()
    assert_frame_equal(new_coder.geo_cache.loc[[0],['query','latitude', 'longitude']], query_result)


def test_get_location_data_cached_query(geo):
    """it pull the data that exists"""
    query = "Chicago, IL"
    cached_row = {
        "query": query,
        "city": "Chicago",
        "state": "Illinois"
    }
    pd.DataFrame([cached_row]).to_csv(geo.cache_path, index=False)
    geo = Geocoder(geo.cache_path)
    geo.geocode = MagicMock()

    result = geo.get_location_data(query)
    assert result['query'] == query
    geo.geocode.assert_not_called()

def test_get_location_data_api_response_location(geo):
    """test API response handling"""
    query = "Chicago, IL"
    mock_location = MagicMock()
    mock_location.latitude = 1
    mock_location.longitude = 2
    mock_location.address = "Chicago, Cook County, Illinois, United States"
    # mock_location.address = {
    #     "city": "Chicago",
    #     "state": "Illinois",
    #     "postcode": "60606",
    #     "country": "USA"
    # }
    geo.geocode = MagicMock(return_value=mock_location)
    result = geo.get_location_data(query)
    
    assert result["query"] == query
    assert result['address'] == "Chicago, Cook County, Illinois, United States"
    assert result['latitude'] == 1
    assert result['longitude'] == 2

    """it saves the data"""
    saved_df = pd.read_csv(geo.cache_path)
    assert len(saved_df) == 1
    assert saved_df.iloc[0]['address'] == "Chicago, Cook County, Illinois, United States"

def test_get_location_data_not_found(geo):
    
    geo.geocode = MagicMock(return_value=None)
    result = geo.get_location_data("Fake Place 12345")
    assert result['latitude'] is None
    saved_df = pd.read_csv(geo.cache_path)
    assert len(saved_df) == 1
