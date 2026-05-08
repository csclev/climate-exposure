import pandas as pd

class ZillowDataParser():
    DTYPES = {
        'RegionID': 'Int64',
        'SizeRank': 'Int64',
        'RegionName': 'object',
        'RegionType': 'object',
        'StateName': 'object',
        'State': 'object',
        'Metro': 'object',
        'StateCodeFIPS': 'Int64',
        'MunicipalCodeFIPS': 'Int64'
    }
    
    META_DATA_COLS = ['RegionID', 'SizeRank', 'RegionName', 'RegionType',
                      'StateName', 'State', 'Metro', 'StateCodeFIPS', 'MunicipalCodeFIPS',
                      'latitude', 'longitude']
    
    def __init__(self, regional_url, national_url):
        self.download_data(regional_url, national_url)
        
    def download_data(self, regional_url, national_url):
        regional_data_raw = pd.read_csv(regional_url)
        national_data_raw = pd.read_csv(national_url)
        national = national_data_raw.loc[national_data_raw["RegionName"] == "United States"]
        self.df = pd.concat([regional_data_raw, national], sort=False, ignore_index=True)
        self.df = self.df.astype(self.DTYPES)
        all_columns = self.df.columns.tolist() + [col for col in self.META_DATA_COLS if col not in self.df.columns]
        self.df = self.df.reindex(columns=all_columns)

    def date_cols(self):
        dates = [col for col in self.df.columns if col not in self.META_DATA_COLS]
        dates = sorted(dates, key=pd.to_datetime)
        return list(dates)

    def get_monthly_panel(self):
        date_cols = self.date_cols()
        long_df = self.df.melt(
            id_vars=self.META_DATA_COLS,
            value_vars=date_cols,
            var_name='date',
            value_name='zhvi'
        )
        long_df['date'] = pd.to_datetime(long_df['date'])
        return long_df
