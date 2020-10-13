gcloud beta compute ssh --zone "asia-southeast1-b" "czsl16" --project "strong-augury-226103"
gcloud beta compute scp --zone "asia-southeast1-b"  --project "strong-augury-226103" czsl16:~/logs/* logs/