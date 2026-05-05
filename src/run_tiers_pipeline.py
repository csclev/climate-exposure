from zillow_pipeline import TiersPipeline
from event_study_tiers import TiersEventStudy

def main():
    datasets = [
        {"neighbor_level": 1,
         "min_neighbors": 1,
         "pre_event_months": 3,
         "post_event_months": 9
         },
        {"neighbor_level": 2,
         "min_neighbors": 2,
         "pre_event_months": 3,
         "post_event_months": 9
         },
        {"neighbor_level": 'r',
         "min_neighbors": 2,
         "pre_event_months": 3,
         "post_event_months": 9
         },
    ]
    pipeline = TiersPipeline()
    for params in datasets:
        print("=======================================")
        print("Building pipeline with ")
        for k, v in params.items():
            print(f"  {k} = {v}")
        dataset_path = pipeline.build(**params)
        tiers = TiersEventStudy(dataset_path=dataset_path, **params)
        tiers.create_analysis_dataset()
        print(f"Data saved to {dataset_path}")
        print("=======================================")

if __name__ == "__main__":
    main()