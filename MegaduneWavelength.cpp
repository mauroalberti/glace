#include <cstdio>
#include <string>
#include <list>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <vector>


using namespace std;



class shot
{
    public:

        int track_id;
        float x, y, z;

    private:

};


class Vector
{
    public:

        long double x, y, z;

        long double magnitude();
        Vector unitvector();

        Vector vectorbyscalar(Vector vector1, long double scalar1);
        Vector vectdiff(Vector vect2);
        Vector vector_projection(Vector vector1, Vector vector2);

        Vector antivector();
        Vector vectorproduct(Vector inputvector2);
        long double scalarproduct(Vector inputvector2);
        long double vectorsangle(Vector inputvector2);

    private:


    protected:


};

long double Vector::magnitude() // magnitude of vector
{
 return sqrt(x*x + y*y + z*z);
}

Vector Vector::unitvector() // normalization of vector
{

 Vector output_vector;

 long double magnit = magnitude();

 output_vector.x = x/magnit;
 output_vector.y = y/magnit;
 output_vector.z = z/magnit;

 return output_vector;
}

Vector Vector::antivector() // vector opposite
{
    Vector antivect;
    antivect.x = -x;
    antivect.y = -y;
    antivect.z = -z;

    return antivect;

}

long double Vector::scalarproduct(Vector inputvector2) // function scalarproduct
{
 long double x1 = x;
 long double y1 = y;
 long double z1 = z;

 long double x2 = inputvector2.x;
 long double y2 = inputvector2.y;
 long double z2 = inputvector2.z;

 //calculations
 return (x1*x2) + (y1*y2) + (z1*z2);

}


long double Vector::vectorsangle(Vector inputvector2) // function vectorsangle output in radians
{

 Vector inputvector1;

 inputvector1.x = x;
 inputvector1.y = y;
 inputvector1.z = z;

 if (inputvector1.magnitude() < 1.0e-5 || inputvector2.magnitude() < 1.0e-5) return -1.0;  // undetermined result, due to too small vector(s)

 long double cos_vectorsangle = inputvector1.scalarproduct(inputvector2)/(inputvector1.magnitude()*inputvector2.magnitude());

 long double vectorsangle_rad;

 if (cos_vectorsangle > 1.0) vectorsangle_rad = 0.0;
 else if  (cos_vectorsangle < -1.0) vectorsangle_rad = M_PI;
 else vectorsangle_rad = acos(cos_vectorsangle);

 return vectorsangle_rad;

}


int main ()
{

    int i,j;


    /* definizione del file di input */
    cout << "Enter input file name: ";
  	string filename;
    cin >> filename;

    ifstream infile;
    infile.open(filename.c_str(), ios::binary);

    if (infile.fail())
    {
    cout << "File di input " << filename << " non trovato\n";
    return 1;
    }
    else
    {
    cout << "File di input riconosciuto e aperto\n";
    }


    // definizione del file di output puntuale
    cout << "Enter output line file name: ";
  	string output_linefilename;
    cin >> output_linefilename;

    ofstream outlinefile;
    outlinefile.open(output_linefilename.c_str(), ios::binary);

    if (outlinefile.fail())
    {
    cout << "File di output " << output_linefilename << " non creato\n";
    return 1;
    }
    else
    {
    cout << "File di output riconosciuto e aperto\n";
    }



    //
    // inizio processamento input dati
    //



    // legge il file di input e crea n file temporanei di processamento,
    // necessari per il calcolo del risultato


    int curr_trackid = -1;
    int progr_tracks = -1;
    int num_recs_read = 0;
    string rec_line;

    int trackid_arr[10000];

    ofstream tempptsfile;
    tempptsfile.open("init.temp", ios::binary);

    int cnt_res_line = 0;

    // legge l'header del file
    getline(infile, rec_line);

    while (!infile.eof())
    {

        // read record
        getline(infile, rec_line);

        if (rec_line.size() == 0) continue;

        istringstream instr(rec_line);
        shot new_shot;
        instr >> new_shot.track_id >> new_shot.x >> new_shot.y;
        // cout << "read " << new_shot.track_id << " " << new_shot.x << " " << new_shot.y << " " << "\n";

        num_recs_read++;

        if (new_shot.track_id != curr_trackid)
        {

            progr_tracks++;

            trackid_arr[progr_tracks] = new_shot.track_id;

            tempptsfile.close();

            ostringstream track_progrcnt_strng;

            track_progrcnt_strng << progr_tracks;

            string temptrackptsfilename = track_progrcnt_strng.str() + ".temp";

            tempptsfile.open(temptrackptsfilename.c_str(), ios::binary);

            cout << temptrackptsfilename << "\n";

            curr_trackid = new_shot.track_id;

        }
        else
        {

            tempptsfile << new_shot.x << " " << new_shot.y << "\n";


        }

    }

    tempptsfile.close();


    // parte di ricerca copppie di punti
    //

    shot curr_shot_A;
    shot prev_shot_A;

    for (int i = 0; i <= progr_tracks; i++)
    {


        ostringstream temp;

        temp << i;

        string tempfilename = temp.str() + ".temp";

        cout << tempfilename << "\n";

        ifstream trackptsAfile;

        trackptsAfile.open(tempfilename.c_str(), ios::in);

        int num_exam_pts_file_A = 0;


        while (!trackptsAfile.eof())
        {

            // read record
            getline(trackptsAfile, rec_line);

            if (rec_line.size() == 0) continue;

            num_exam_pts_file_A++;

             if (num_exam_pts_file_A > 1)
            {

                prev_shot_A.x = curr_shot_A.x;
                prev_shot_A.y = curr_shot_A.y;

            }


            istringstream instr(rec_line);
            instr >> curr_shot_A.x >> curr_shot_A.y;


            if (num_exam_pts_file_A == 1)
            {
                continue;
            }


            // definisce vettore locale traccia
            Vector localtrackvector;
            localtrackvector.x = curr_shot_A.x - prev_shot_A.x;
            localtrackvector.y = curr_shot_A.y - prev_shot_A.y;
            localtrackvector.z = 0;

            // parte di confronto con tutti i records di tutti gli altri file

            double min_distance_pt_1 = 99999999.9999;

            shot nearest_shot_1;
            nearest_shot_1.track_id = -999;
            nearest_shot_1.x = -999999.9;
            nearest_shot_1.y = -999999.9;




            int id_file_nearest= -99;
            Vector nearest_vector; nearest_vector.x=0.0; nearest_vector.y=0.0; nearest_vector.z=0.0;

            bool found_nearest_point = false;



            // ricerca il singolo punto più vicino

            for (int j = 0; j <= progr_tracks; j++)
            {

                if (j == i) continue;

                ostringstream temp2;

                temp2 << j;

                string temp2filename = temp2.str() + ".temp";

                cout << temp2filename << "\n";

                ifstream trackptsBfile;

                trackptsBfile.open(temp2filename.c_str(), ios::in);

                while (!trackptsBfile.eof())
                {

                    // read record
                    getline(trackptsBfile, rec_line);

                    if (rec_line.size() == 0) continue;

                    istringstream instr(rec_line);

                    shot curr_shot_B;
                    instr >> curr_shot_B.x >> curr_shot_B.y;

                    Vector currseparationvector;
                    currseparationvector.x = curr_shot_B.x - curr_shot_A.x;
                    currseparationvector.y = curr_shot_B.y - curr_shot_A.y;
                    currseparationvector.z = 0;

                    float angle_track_separ_degr = (localtrackvector.vectorsangle(currseparationvector))*180.0/M_PI;

                    if (angle_track_separ_degr < 60.0 || angle_track_separ_degr > 120.0) continue;

                    double delta_x_squared = (curr_shot_A.x - curr_shot_B.x)*(curr_shot_A.x - curr_shot_B.x);
                    double delta_y_squared = (curr_shot_A.y - curr_shot_B.y)*(curr_shot_A.y - curr_shot_B.y);

                    float my_curr_distance = sqrt(delta_x_squared + delta_y_squared);


                    if (my_curr_distance < min_distance_pt_1)
                    {

                        id_file_nearest = j;

                        nearest_shot_1.track_id = trackid_arr[j];
                        nearest_shot_1.x = curr_shot_B.x;
                        nearest_shot_1.y = curr_shot_B.y;
                        min_distance_pt_1 = my_curr_distance;

                    }


                }

                trackptsBfile.close();


            }

            if (min_distance_pt_1 < 10000.0)
            {

               found_nearest_point = true;
               nearest_vector.x = nearest_shot_1.x - curr_shot_A.x;
               nearest_vector.y = nearest_shot_1.y - curr_shot_A.y;

                cnt_res_line++;
                outlinefile << cnt_res_line << "\n";
                outlinefile << curr_shot_A.x << " " << curr_shot_A.y << "\n";
                outlinefile << nearest_shot_1.x << " " << nearest_shot_1.y << "\n";
                outlinefile << "END\n";


            }


            if (!found_nearest_point) continue;


            // ricerca il punto più vicino nella seconda traccia

            double min_distance_pt_2 = 99999999.9999;

            shot nearest_shot_2;
            nearest_shot_2.track_id = -999;
            nearest_shot_2.x = -999999.9;
            nearest_shot_2.y = -999999.9;

            for (int j = 0; j <= progr_tracks; j++)
            {

                if (j == i || j == id_file_nearest) continue;

                ostringstream temp2;

                temp2 << j;

                string temp2filename = temp2.str() + ".temp";

                cout << temp2filename << "\n";

                ifstream trackptsBfile;

                trackptsBfile.open(temp2filename.c_str(), ios::in);

                while (!trackptsBfile.eof())
                {

                    // read record
                    getline(trackptsBfile, rec_line);

                    if (rec_line.size() == 0) continue;

                    istringstream instr(rec_line);


                    shot curr_shot_B;
                    instr >> curr_shot_B.x >> curr_shot_B.y;

                    Vector currseparationvector;
                    currseparationvector.x = curr_shot_B.x - curr_shot_A.x;
                    currseparationvector.y = curr_shot_B.y - curr_shot_A.y;
                    currseparationvector.z = 0;

                    float angle_track_separ_degr = (localtrackvector.vectorsangle(currseparationvector))*180.0/M_PI;

                    if (angle_track_separ_degr < 60.0 || angle_track_separ_degr > 120.0) continue;


                    float angle_separation_vectors = currseparationvector.vectorsangle(nearest_vector)*180.0/M_PI;

                    if (angle_separation_vectors < 120.0) continue;

                    double delta_x_squared = (curr_shot_A.x - curr_shot_B.x)*(curr_shot_A.x - curr_shot_B.x);
                    double delta_y_squared = (curr_shot_A.y - curr_shot_B.y)*(curr_shot_A.y - curr_shot_B.y);

                    float my_curr_distance = sqrt(delta_x_squared + delta_y_squared);


                    if (my_curr_distance < min_distance_pt_2)
                    {

                        nearest_shot_2.track_id = trackid_arr[j];
                        nearest_shot_2.x = curr_shot_B.x;
                        nearest_shot_2.y = curr_shot_B.y;
                        min_distance_pt_2 = my_curr_distance;

                    }


                }

                trackptsBfile.close();


            }

            if (min_distance_pt_2 < 10000.0)
            {


                cnt_res_line++;
                outlinefile << cnt_res_line << "\n";
                outlinefile << curr_shot_A.x << " " << curr_shot_A.y << "\n";
                outlinefile << nearest_shot_2.x << " " << nearest_shot_2.y << "\n";
                outlinefile << "END\n";


            }

        }


        trackptsAfile.close();

    }


    infile.close();

    outlinefile << "END\n";

    outlinefile.close();

    return 0;

}




