import pandas as pd
import numpy as np
from pathlib import Path
import urllib
from zillow_data_parser import ZillowDataParser

KARL_KOSS_REGIONS = {
    '01': 'Southeast', '12': 'Southeast', '13': 'Southeast', '28': 'Southeast',
    '37': 'Southeast', '45': 'Southeast', '47': 'Southeast',
    '05': 'South', '22': 'South', '40': 'South', '48': 'South',
    '04': 'Southwest', '06': 'Southwest', '32': 'Southwest', '35': 'Southwest',
    '08': 'West', '16': 'West', '30': 'West', '49': 'West',
    '41': 'Northwest', '53': 'Northwest',
    '19': 'Upper Midwest', '20': 'Upper Midwest', '27': 'Upper Midwest',
    '31': 'Upper Midwest', '17': 'Upper Midwest', '29': 'Upper Midwest',
    '38': 'Upper Midwest', '46': 'Upper Midwest',
    '18': 'Ohio Valley', '21': 'Ohio Valley', '24': 'Ohio Valley',
    '39': 'Ohio Valley', '42': 'Ohio Valley', '51': 'Ohio Valley',
    '54': 'Ohio Valley', '11': 'Ohio Valley',
    '09': 'Northeast', '23': 'Northeast', '25': 'Northeast',
    '33': 'Northeast', '34': 'Northeast', '36': 'Northeast',
    '44': 'Northeast', '50': 'Northeast', '10': 'Northeast',
}

class TiersPipeline():

    zillow_county_url        = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
    zillow_county_top_url    = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.67_1.0_sm_sa_month.csv"
    zillow_county_bottom_url = "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.0_0.33_sm_sa_month.csv"
    zillow_msa_url           = "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

    def __init__(self):
        self.raw_path = Path('../data/raw')
        self.raw_path.mkdir(parents=True, exist_ok=True)
        self.processed_path = Path('../data/processed')
        self.processed_path.mkdir(parents=True, exist_ok=True)
        self.load_zillow_data()
        self.load_adjacent_df()
        self.load_affected()

    def build(self, neighbor_level, min_neighbors, pre_event_months, post_event_months):
        level_str = 'r' if neighbor_level == 'r' else str(neighbor_level)
        dataset_path = Path(self.processed_path / f"nlevel{level_str}_nmin{min_neighbors}_pre{pre_event_months}_post{post_event_months}")
        dataset_path.mkdir(exist_ok=True, parents=True)

        zillow_mid    = ZillowDataParser(str(self.county_mid_path), str(self.msa_path))
        zillow_top    = ZillowDataParser(str(self.county_top_path), str(self.msa_path))
        zillow_bottom = ZillowDataParser(str(self.county_bottom_path), str(self.msa_path))

        panel_mid,    zhvi_idx_mid    = self.build_panel(zillow_mid)
        panel_top,    zhvi_idx_top    = self.build_panel(zillow_top)
        panel_bottom, zhvi_idx_bottom = self.build_panel(zillow_bottom)

        zhvi_idx_mid.to_pickle(dataset_path / 'zhvi_idx_mid.pkl')
        zhvi_idx_top.to_pickle(dataset_path / 'zhvi_idx_top.pkl')
        zhvi_idx_bottom.to_pickle(dataset_path / 'zhvi_idx_bottom.pkl')

        # Build a single neighbor dict using mid-tier coverage as the canonical pool
        # All three tiers use the same neighbor set; complete-window filtering in
        # create_baseline_lookup drops neighbors missing data in the specific tier
        neighbors = self.create_neighbors(zhvi_idx_mid, neighbor_level)

        baseline_lookup_mid    = self.create_baseline_lookup(zhvi_idx_mid,    neighbors, min_neighbors, pre_event_months, post_event_months)
        baseline_lookup_top    = self.create_baseline_lookup(zhvi_idx_top,    neighbors, min_neighbors, pre_event_months, post_event_months)
        baseline_lookup_bottom = self.create_baseline_lookup(zhvi_idx_bottom, neighbors, min_neighbors, pre_event_months, post_event_months)

        for name, baseline_lookup in [
            ('mid',    baseline_lookup_mid),
            ('top',    baseline_lookup_top),
            ('bottom', baseline_lookup_bottom)
        ]:
            assert baseline_lookup['target_fips'].str.len().eq(5).all(), f'{name}: FIPS not all 5 digits'
            assert baseline_lookup['t'].between(-pre_event_months, post_event_months).all(), f'{name}: Offset t out of window range'
            assert baseline_lookup.duplicated(['target_fips', 'storm_year', 'storm_month', 't']).sum() == 0, f'{name}: Duplicate event-offset rows'
            missing = baseline_lookup['baseline_zhvi'].isnull().sum()
            if missing > 0:
                print(f'Warning ({name}): {missing:,} rows missing baseline_zhvi')
            else:
                print(f'{name}: All baseline ZHVI present')
        self.export_baseline(baseline_lookup_mid, dataset_path, 'mid', zhvi_idx_mid)
        self.export_baseline(baseline_lookup_top, dataset_path, 'top', zhvi_idx_top)
        self.export_baseline(baseline_lookup_bottom, dataset_path, 'bottom', zhvi_idx_bottom)

        return dataset_path

    def export_baseline(self, baseline_lookup, dataset_path, suff, zhvi_idx):
        baseline_lookup.to_pickle(dataset_path / f"baseline_lookup_{suff}.pkl")
        
        event_neighbors = (
            baseline_lookup
            .drop_duplicates(['target_fips', 'storm_year', 'storm_month'])
            [['target_fips', 'storm_year', 'storm_month', 'n_clean_neighbors', 'neighbor_fips']]
            .copy()
        )
        
        # Target ZHVI at T=0
        event_neighbors['target_zhvi_t0'] = event_neighbors.apply(
            lambda r: zhvi_idx.get((r['target_fips'], r['storm_year'], r['storm_month'])), axis=1
        )
        
        # Mean baseline ZHVI at T=0
        def mean_neighbor_zhvi(row):
            fips_list = [f.strip() for f in row['neighbor_fips'].split(',')]
            vals = [zhvi_idx.get((f, row['storm_year'], row['storm_month'])) for f in fips_list]
            vals = [v for v in vals if v is not None and not pd.isna(v)]
            return np.mean(vals) if vals else np.nan
        
        event_neighbors['baseline_mean_zhvi_t0'] = event_neighbors.apply(mean_neighbor_zhvi, axis=1)
        
        event_neighbors.to_csv(dataset_path / f"event_neighbors_{suff}.csv", index=False)
        
        print(f"Saved baseline_lookup_{suff}.pkl")
        print(f"Saved event_neighbors_{suff}.csv ({len(event_neighbors):,} events)")
        print(f'Shape: {baseline_lookup.shape}')
        print(f'Storm events covered: {baseline_lookup.groupby(["target_fips","storm_year","storm_month"]).ngroups:,}')

    def create_neighbors(self, zhvi_idx, neighbor_level):
        zhvi_fips = set(zhvi_idx.index.get_level_values('stcofips'))

        if neighbor_level == 1:
            neighbors = self.adj_df[
                self.adj_df['Neighbor GEOID'] != self.adj_df['County GEOID']
            ].groupby('County GEOID')['Neighbor GEOID'].apply(list).to_dict()

        elif neighbor_level == 2:
            layer1 = self.adj_df[
                self.adj_df['Neighbor GEOID'] != self.adj_df['County GEOID']
            ][["County GEOID", "Neighbor GEOID"]]
            layer1_dict = layer1.groupby('County GEOID')['Neighbor GEOID'].apply(set).to_dict()

            neighbors_df = layer1.merge(
                layer1,
                left_on  = 'Neighbor GEOID',
                right_on = 'County GEOID'
            )
            neighbors_df = neighbors_df.drop(["Neighbor GEOID_x", "County GEOID_y"], axis=1)
            neighbors_df = neighbors_df.rename(columns={"County GEOID_x": "target", "Neighbor GEOID_y": "layer2"})
            neighbors_df = neighbors_df[
                (neighbors_df["target"] != neighbors_df["layer2"]) &
                (~neighbors_df.apply(lambda r: r["layer2"] in layer1_dict.get(r["target"], set()), axis=1))
            ]
            neighbors = neighbors_df.groupby("target")["layer2"].apply(list).to_dict()

        elif neighbor_level == 'r':
            # Build region membership from FIPS
            # stcofips is 5-char string, state_fips is first 2 chars
            region_to_counties = {}
            for fips in zhvi_fips:
                state_fips = fips[:2]
                region = KARL_KOSS_REGIONS.get(state_fips)
                if region is None:
                    continue
                region_to_counties.setdefault(region, []).append(fips)

            # For each county, neighbors are all other counties in same region
            neighbors = {}
            for fips in zhvi_fips:
                state_fips = fips[:2]
                region = KARL_KOSS_REGIONS.get(state_fips)
                if region is None:
                    continue
                neighbors[fips] = [
                    c for c in region_to_counties.get(region, [])
                    if c != fips  # exclude self
                ]
        else:
            raise ValueError(f"neighbor_level must be 1, 2, or 'r', got {neighbor_level}")

        return {
            target: [n for n in neighbor_list if n in zhvi_fips]
            for target, neighbor_list in neighbors.items()
        }

    def create_baseline_lookup(self, zhvi_idx, neighbors, min_neighbors, pre_event_months, post_event_months):
        rows    = []
        flagged = 0

        for (target_fips, storm_year, storm_month) in self.affected:
            neighbor_list = neighbors.get(target_fips, [])
            if not neighbor_list:
                continue

            window = self.get_window_months(storm_year, storm_month, pre_event_months, post_event_months)

            clean_neighbors = [
                n for n in neighbor_list
                if n != target_fips and  # fallback self-exclusion
                all((n, yr, mo) not in self.affected for (_, yr, mo) in window)
            ]

            complete_neighbors = []
            for n in clean_neighbors:
                zhvi_t0 = zhvi_idx.get((n, storm_year, storm_month))
                if zhvi_t0 is None or pd.isna(zhvi_t0) or zhvi_t0 == 0:
                    continue
                if all(
                    zhvi_idx.get((n, yr, mo)) is not None and
                    not pd.isna(zhvi_idx.get((n, yr, mo)))
                    for (_, yr, mo) in window
                ):
                    complete_neighbors.append(n)

            complete_neighbors = list(set(complete_neighbors))

            n_clean = len(complete_neighbors)
            if n_clean < min_neighbors:
                flagged += 1
            if n_clean == 0:
                continue

            for (t, yr, mo) in window:
                indexed_vals = []
                for n in complete_neighbors:
                    zhvi_t0 = zhvi_idx.get((n, storm_year, storm_month))
                    zhvi_t  = zhvi_idx.get((n, yr, mo))
                    indexed_vals.append((zhvi_t / zhvi_t0) * 100)

                rows.append({
                    'target_fips':       target_fips,
                    'storm_year':        storm_year,
                    'storm_month':       storm_month,
                    't':                 t,
                    'year':              yr,
                    'month':             mo,
                    'baseline_zhvi':     np.mean(indexed_vals),
                    'n_clean_neighbors': n_clean,
                    'neighbor_fips':     ','.join(complete_neighbors)
                })

        baseline_lookup = pd.DataFrame(rows)
        print(f'Total baseline rows:                    {len(baseline_lookup):,}')
        print(f'Events with <{min_neighbors} clean neighbors (flagged): {flagged:,}')
        print(f'Storm events covered:                   {baseline_lookup.groupby(["target_fips","storm_year","storm_month"]).ngroups:,}')
        return baseline_lookup

    def get_window_months(self, storm_year, storm_month, pre, post):
        storm_date = pd.Timestamp(year=storm_year, month=storm_month, day=1)
        return [
            (t, (storm_date + pd.DateOffset(months=t)).year,
                (storm_date + pd.DateOffset(months=t)).month)
            for t in range(-pre, post + 1)
        ]

    def build_panel(self, zillow):
        panel = zillow.get_monthly_panel()
        panel = panel[panel['RegionType'] == 'county'].copy()
        panel['state_fips']  = panel['StateCodeFIPS'].astype(str).str.zfill(2)
        panel['county_fips'] = panel['MunicipalCodeFIPS'].astype(str).str.zfill(3)
        panel['stcofips']    = panel['state_fips'] + panel['county_fips']
        panel['year']  = panel['date'].dt.year
        panel['month'] = panel['date'].dt.month
        EXCLUDE_STATES = {'02', '15', '72'}
        panel = panel[~panel['state_fips'].isin(EXCLUDE_STATES)].copy()
        zhvi_idx = panel.set_index(['stcofips', 'year', 'month'])['zhvi']
        panel = panel[panel['year'].between(2020, 2025)].copy()
        n_before = len(panel)
        panel = panel.dropna(subset=['zhvi'])
        print(f'Dropped {n_before - len(panel):,} rows with missing ZHVI or unassigned region')
        print(f'Panel shape: {panel.shape}')
        return panel, zhvi_idx

    def load_zillow_data(self):
        self.county_mid_path    = self.raw_path / 'zillow_county_mid_zhvi.csv'
        self.county_top_path    = self.raw_path / 'zillow_county_top_zhvi.csv'
        self.county_bottom_path = self.raw_path / 'zillow_county_bottom_zhvi.csv'
        self.msa_path           = self.raw_path / 'zillow_msa_zhvi.csv'

        if not self.county_mid_path.exists():
            urllib.request.urlretrieve(self.zillow_county_url, self.county_mid_path)
            print('Downloaded mid tier')
        else:
            print('Using cached mid tier')

        if not self.county_top_path.exists():
            urllib.request.urlretrieve(self.zillow_county_top_url, self.county_top_path)
            print('Downloaded top tier')
        else:
            print('Using cached top tier')

        if not self.county_bottom_path.exists():
            urllib.request.urlretrieve(self.zillow_county_bottom_url, self.county_bottom_path)
            print('Downloaded bottom tier')
        else:
            print('Using cached bottom tier')

        if not self.msa_path.exists():
            urllib.request.urlretrieve(self.zillow_msa_url, self.msa_path)
            print('Downloaded MSA')
        else:
            print('Using cached MSA')

    def load_affected(self):
        storms = pd.read_pickle(self.processed_path / 'storm_events.pkl')
        self.affected = set(zip(storms['stcofips'], storms['year'], storms['month']))

    def load_adjacent_df(self):
        self.adj_df = pd.read_csv(self.raw_path / 'county_adjacency.csv')
        self.adj_df['County GEOID'] = self.adj_df['County GEOID'].astype(int).astype(str).str.zfill(5)
        self.adj_df['Neighbor GEOID'] = self.adj_df['Neighbor GEOID'].astype(int).astype(str).str.zfill(5)
