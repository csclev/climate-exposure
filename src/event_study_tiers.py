import pandas as pd
import numpy as np
from pathlib import Path
from collections import Counter

class TiersEventStudy():


    LATEST_ZHVI = pd.Timestamp('2026-02-28')

    OUTPUT_COLS = [
        'stcofips', 'year', 'month', 'tier', 'event_type',
        'auc', 'auc_variance', 'pre_trend_annual',
        'event_count', 'total_damage', 'log_damage', 'total_duration_days', 'episode_count',
        'resl_score', 'resl_value', 'risk_value', 'eal_valt', 'sovi_score', 'nri_vintage',
    ]
    NRI_COLS = ['stcofips', 'storm_year', 'month', 'nri_vintage', 'resl_score', 'resl_value','risk_value', 'eal_valt', 'sovi_score']


    def __init__(self, dataset_path, neighbor_level, min_neighbors, pre_event_months, post_event_months):
        self.raw_path = Path('../data/raw')
        self.processed_path = Path('../data/processed')
        self.neighbor_level = neighbor_level
        self.min_neighbors = min_neighbors
        self.pre_event_months = pre_event_months
        self.post_event_months = post_event_months
        assert dataset_path == self._path_check()
        self.dataset_path = dataset_path
        self.load_processed_data()

    def load_processed_data(self):
        self.storms   = pd.read_pickle(self.processed_path /'storm_events.pkl')
        self.nri      = pd.read_pickle(self.processed_path / 'nri_panel_smooth.pkl')
        self.zhvi_idx_mid    = pd.read_pickle(self.dataset_path / 'zhvi_idx_mid.pkl')
        self.zhvi_idx_top    = pd.read_pickle(self.dataset_path / 'zhvi_idx_top.pkl')
        self.zhvi_idx_bottom = pd.read_pickle(self.dataset_path / 'zhvi_idx_bottom.pkl')
        print(f'Storm events:  {self.storms.shape}')
        print(f'NRI panel:     {self.nri.shape}')
        print(f'ZHVI idx mid:    {len(self.zhvi_idx_mid):,}')
        print(f'ZHVI idx top:    {len(self.zhvi_idx_top):,}')
        print(f'ZHVI idx bottom: {len(self.zhvi_idx_bottom):,}')

        baseline_lookup_mid    = pd.read_pickle(self.dataset_path / 'baseline_lookup_mid.pkl')
        baseline_lookup_top    = pd.read_pickle(self.dataset_path / 'baseline_lookup_top.pkl')
        baseline_lookup_bottom = pd.read_pickle(self.dataset_path / 'baseline_lookup_bottom.pkl')
        self.baseline_idx_mid    = baseline_lookup_mid.set_index(['target_fips', 'storm_year', 'storm_month', 't'])['baseline_zhvi']
        self.baseline_idx_top    = baseline_lookup_top.set_index(['target_fips', 'storm_year', 'storm_month', 't'])['baseline_zhvi']
        self.baseline_idx_bottom = baseline_lookup_bottom.set_index(['target_fips', 'storm_year', 'storm_month', 't'])['baseline_zhvi']


    def create_analysis_dataset(self,nri_cols=NRI_COLS):
        eligible = self.filter_storms()
        metrics_df = self.create_tier_results(eligible)
        metrics_df = self.join_storm_nri_data(metrics_df, nri_cols=nri_cols)
        metrics_df = self.output_monthly_deviations_df(metrics_df)
        self.output_analysis_data(metrics_df)

    def output_analysis_data(self, metrics_df):
        out = metrics_df[self.OUTPUT_COLS].sort_values(['stcofips', 'year', 'month', 'tier']).reset_index(drop=True)

        # Filter to events with all 3 tiers present
        tier_counts = out.groupby(['stcofips', 'year', 'month'])['tier'].count()
        complete    = tier_counts[tier_counts == 3].reset_index()[['stcofips', 'year', 'month']]
        out         = out.merge(complete, on=['stcofips', 'year', 'month'], how='inner')

        out.to_pickle(self.dataset_path / 'analysis_dataset.pkl')
        out.to_csv(self.dataset_path / 'analysis_dataset.csv', index=False)
        print(f'Exported analysis_dataset.pkl and analysis_dataset.csv to {self.dataset_path}')
        print(f'Shape: {out.shape}')
        print(f'Tier counts: {out["tier"].value_counts().to_dict()}')

    def join_storm_nri_data(self, metrics_df, nri_cols):
        metrics_df = metrics_df.merge(
            self.nri[nri_cols].rename(columns={'storm_year': 'year'}),
            on=['stcofips', 'year', 'month'],
            how='left'
        )
        print(f'Shape after joins: {metrics_df.shape}')
        print(f'Missing NRI: {metrics_df["resl_score"].isnull().sum()}')
        missing_nri = metrics_df['resl_score'].isnull()
        print(f'Dropping {missing_nri.sum()} rows with missing NRI (CT county restructuring edge case)')
        metrics_df = metrics_df[~missing_nri].copy()

        assert metrics_df.duplicated(['stcofips', 'year', 'month', 'tier']).sum() == 0, 'Duplicate county-month-tier rows'
        assert metrics_df['auc'].notnull().all(), 'Null AUC values'
        assert metrics_df['auc_variance'].notnull().all(), 'Null AUC variance values'
        assert metrics_df['resl_score'].isnull().sum() == 0, 'Missing NRI scores'

        print('All assertions passed')
        print(f'\nFinal dataset summary by tier:')
        print(metrics_df.groupby('tier')[['auc', 'auc_variance', 'pre_trend_annual', 'log_damage', 'event_count']].describe().round(2))
        return metrics_df

    def output_monthly_deviations_df(self, metrics_df):
        monthly_rows = []
        for _, row in metrics_df.iterrows():
            for t, dev in enumerate(row['post_deviations'], start=1):
                monthly_rows.append({
                    'stcofips': row['stcofips'],
                    'year':     row['year'],
                    'month':    row['month'],
                    'tier':     row['tier'],
                    'month_t':  t,
                    'deviation': dev,
                })

        for _, row in metrics_df.iterrows():
            monthly_rows.append({
                'stcofips': row['stcofips'],
                'year':     row['year'],
                'month':    row['month'],
                'tier':     row['tier'],
                'month_t':  0,
                'deviation': 0.0,
            })

        monthly_deviations = pd.DataFrame(monthly_rows)

        pre_rows = []
        for _, row in metrics_df.iterrows():
            for t, dev in enumerate(row['pre_deviations'], start=1):
                if not np.isnan(dev):
                    pre_rows.append({
                        'stcofips': row['stcofips'],
                        'year':     row['year'],
                        'month':    row['month'],
                        'tier':     row['tier'],
                        'month_t':  -t,
                        'deviation': dev,
                    })

        pre_deviations_df = pd.DataFrame(pre_rows)
        all_deviations = pd.concat([pre_deviations_df, monthly_deviations], ignore_index=True)
        all_deviations.to_csv(self.dataset_path / 'monthly_deviations.csv', index=False)
        print(f'Exported monthly_deviations.csv: {all_deviations.shape} to {self.dataset_path}')
        metrics_df = metrics_df.drop(columns=['post_deviations', 'pre_deviations'])
        print(f'Dropped cols: post_deviations, pre_deviations')
        return metrics_df


    def filter_storms(self):
        # Convert year/month to period for cutoff comparison
        self.storms['period'] = pd.to_datetime(
            self.storms[['year', 'month']].assign(day=1)
        )

        cutoff      = self.LATEST_ZHVI - pd.DateOffset(months=self.post_event_months)
        eligible    = self.storms[self.storms['period'] <= cutoff].copy()
        n_eligible  = len(eligible)

        # --- Isolation filter: drop events with another storm in the PRE/POST window ---
        storms_ref = self.storms[['stcofips', 'year', 'month', 'period']].copy()

        neighbors = eligible.merge(
            storms_ref.rename(columns={
                'year': 'other_year',
                'month': 'other_month',
                'period': 'other_period'
            }),
            on='stcofips',
            how='left'
        )

        neighbors = neighbors[
            ~((neighbors['year'] == neighbors['other_year']) &
              (neighbors['month'] == neighbors['other_month']))
        ]

        neighbors['months_offset'] = (
            (neighbors['other_period'].dt.year  - neighbors['period'].dt.year) * 12 +
            (neighbors['other_period'].dt.month - neighbors['period'].dt.month)
        )

        contaminated = neighbors[
            (neighbors['months_offset'] >= -self.pre_event_months) &
            (neighbors['months_offset'] <=  self.post_event_months)
        ][['stcofips', 'year', 'month']].drop_duplicates()

        eligible = eligible.merge(
            contaminated.assign(_flag=True),
            on=['stcofips', 'year', 'month'],
            how='left'
        )

        eligible   = eligible[eligible['_flag'].isna()].drop(columns=['_flag']).copy()
        n_isolated = len(eligible)

        # --- Damage filter: drop events with no measured damage ---
        # isolated = isolated[isolated['total_damage'] > 0].copy()
        # n_damage = len(isolated)

        print(f'Total storm events:               {len(self.storms):,}')
        print(f'Eligible (complete window):        {n_eligible:,}')
        print(f'Isolated (no spillover):           {n_isolated:,}  dropped: {n_eligible - n_isolated:,}  ({(n_eligible - n_isolated)/n_eligible:.1%})')
        # print(f'With measured damage:              {n_damage:,}  dropped: {n_isolated - n_damage:,}  ({(n_isolated - n_damage)/n_isolated:.1%})')
        print(f'Survival rate (total → final):     {n_isolated/len(self.storms):.1%}')
        print()
        print(f"Using {len(eligible)} samples")
        return eligible

    def create_tier_results(self, eligible):
        tier_results = []

        for tier, zhvi_idx, baseline_idx in [
            ('mid',    self.zhvi_idx_mid, self.baseline_idx_mid),
            ('top',    self.zhvi_idx_top,    self.baseline_idx_top),
            ('bottom', self.zhvi_idx_bottom, self.baseline_idx_bottom)
        ]:
            results     = []
            drop_reasons = Counter()
            
            for _, row in eligible.iterrows():
                metrics = self.compute_event_metrics(row, zhvi_idx, baseline_idx, drop_reasons)
                if metrics is not None:
                    results.append({**row.to_dict(), **metrics})
            
            print(f'\n--- {tier} ---')
            print(f'Events with complete windows: {len(results):,}')
            print(f'Drop reasons:')
            for reason, count in sorted(drop_reasons.items(), key=lambda x: -x[1]):
                print(f'  {reason}: {count:,}')
            
            tier_df = pd.DataFrame(results)
            tier_df['tier'] = tier
            tier_results.append(tier_df)

        return pd.concat(tier_results, ignore_index=True)
        print(f'\nTotal rows across all tiers: {len(metrics_df):,}')
        print(f'Tiers: {metrics_df["tier"].value_counts().to_dict()}')

    def compute_event_metrics(self, row, zhvi_idx, baseline_idx, drop_reasons):
        fips        = row['stcofips']
        storm_year  = row['year']
        storm_month = row['month']
        storm_date  = pd.Timestamp(year=storm_year, month=storm_month, day=1)

        # T=0 anchor — county ZHVI only, baseline is already indexed to 100
        county_zhvi_t0   = zhvi_idx.get((fips, storm_year, storm_month))
        baseline_zhvi_t0 = baseline_idx.get((fips, storm_year, storm_month, 0))

        if county_zhvi_t0 is None:
            drop_reasons['county_zhvi_t0_missing'] += 1
            return None
        if baseline_zhvi_t0 is None:
            drop_reasons['baseline_zhvi_t0_missing'] += 1
            return None
        if pd.isna(county_zhvi_t0):
            drop_reasons['county_zhvi_t0_nan'] += 1
            return None
        if pd.isna(baseline_zhvi_t0):
            drop_reasons['baseline_zhvi_t0_nan'] += 1
            return None
        # Assert baseline is indexed to 100 at T=0
        assert abs(baseline_zhvi_t0 - 100) < 0.01, f'Baseline T=0 is {baseline_zhvi_t0}, expected 100'

        # Post-storm window
        post_deviations = []
        for t in range(1, self.post_event_months + 1):
            future = storm_date + pd.DateOffset(months=t)
            county_zhvi   = zhvi_idx.get((fips, future.year, future.month))
            baseline_zhvi = baseline_idx.get((fips, storm_year, storm_month, t))
            if county_zhvi is None:
                drop_reasons[f'post_county_zhvi_missing_t{t}'] += 1
                return None
            if baseline_zhvi is None:
                drop_reasons[f'post_baseline_missing_t{t}'] += 1
                return None
            if pd.isna(county_zhvi) or pd.isna(baseline_zhvi):
                drop_reasons[f'post_nan_t{t}'] += 1
                return None
            # County indexed to 100 at T=0, baseline already indexed
            county_idx = (county_zhvi / county_zhvi_t0) * 100
            post_deviations.append(county_idx - baseline_zhvi)

        # Pre-storm window
        pre_deviations = []
        for t in range(1, self.pre_event_months + 1):
            past = storm_date - pd.DateOffset(months=t)
            county_zhvi   = zhvi_idx.get((fips, past.year, past.month))
            baseline_zhvi = baseline_idx.get((fips, storm_year, storm_month, -t))
            if county_zhvi is None or baseline_zhvi is None or pd.isna(county_zhvi) or pd.isna(baseline_zhvi):
                pre_deviations.append(np.nan)
            else:
                county_idx = (county_zhvi / county_zhvi_t0) * 100
                pre_deviations.append(county_idx - baseline_zhvi)

        post_arr = np.array(post_deviations)
        pre_arr  = np.array(pre_deviations)

        auc               = float(np.sum(post_arr))
        auc_variance      = float(np.std(post_arr, ddof=1))
        pre_trend_monthly = float(np.nanmean(pre_arr)) if not np.all(np.isnan(pre_arr)) else np.nan
        pre_trend_annual  = pre_trend_monthly * 12/self.pre_event_months if not np.isnan(pre_trend_monthly) else np.nan

        return {
            'auc':              auc,
            'auc_variance':     auc_variance,
            'pre_trend_annual': pre_trend_annual,
            'post_deviations':  post_arr.tolist(),
            'pre_deviations':   pre_arr.tolist(),
        }
    def _path_check(self):
        level_str = 'r' if self.neighbor_level == 'r' else str(self.neighbor_level)
        return self.processed_path / f"nlevel{level_str}_nmin{self.min_neighbors}_pre{self.pre_event_months}_post{self.post_event_months}"