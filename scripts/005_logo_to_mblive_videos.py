import os
import cv2


# Define paths to input and output directories
input_dir = "outputs/video_material/mb_live"
output_dir = "outputs/video_material/mb_live_output"

# Define path to the FishID logo
logo_path = "outputs/video_material/FishID_Logo_V7_SL.png"

# Define function to add logo to video frames
def add_logo_to_frame(frame, logo):
    # Resize logo to a width of 100 pixels
    logo = cv2.resize(logo, (100, int(logo.shape[0] * 100 / logo.shape[1])))
    
    # Convert logo to RGB if it has an alpha channel
    if logo.shape[2] == 4:
        logo = cv2.cvtColor(logo, cv2.COLOR_RGBA2RGB)
    
    # Add logo to top right corner of frame
    logo_height, logo_width, _ = logo.shape
    frame[0:logo_height, -logo_width:, :] = logo
    
    return frame


# Loop through all files in the input directory (including subdirectories)
for root, dirs, files in os.walk(input_dir):
    for file in files:
        # Check if file is an MP4 file
        if file.endswith(".MP4"):
            # Define paths to input and output files
            input_path = os.path.join(root, file)
            output_path = os.path.join(output_dir, os.path.relpath(input_path, input_dir)).replace(".MP4", "_FishID_logo.MP4")
            
            # Create output directory if it doesn't exist
            os.makedirs(os.path.dirname(output_path), exist_ok=True)
            
            # Load FishID logo
            logo = cv2.imread(logo_path, cv2.IMREAD_UNCHANGED)
            
            # Open input video file
            video = cv2.VideoCapture(input_path)
            
            # Get video dimensions and frames per second
            width = int(video.get(cv2.CAP_PROP_FRAME_WIDTH))
            height = int(video.get(cv2.CAP_PROP_FRAME_HEIGHT))
            fps = int(video.get(cv2.CAP_PROP_FPS))
            
            # Define codec for output video file
            fourcc = cv2.VideoWriter_fourcc(*"mp4v")
            
            # Create output video file
            out = cv2.VideoWriter(output_path, fourcc, fps, (width, height))
            
            # Loop through all frames in input video file
            while True:
                # Read next frame
                ret, frame = video.read()
                
                # Check if frame was read successfully
                if not ret:
                    break
                
                # Add FishID logo to frame
                frame = add_logo_to_frame(frame, logo)
                
                # Write frame to output video file
                out.write(frame)
            
            # Release input and output video files
            video.release()
            out.release()
            
            print(f"Processed {input_path}")
