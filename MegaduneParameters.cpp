#include <cstdio>
#include <string>
#include <list>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <vector>
#include <set>
#include <map>
#include <iomanip>


using namespace std;

class input_datum
{
    public:
     int id_rec, id_track;
     double x, y, s, h, z_orig, z_linint, z_proxint, dist_proxelpt;

    private:

};


class Point
{
    public:

        Point();
        Point(int id,  double x,  double y,  double z);
        int id;
        double x, y, z;

        Point&
        operator=(Point p1)
        {
            id = p1.id;
            x = p1.x;
            y = p1.y;
            z = p1.z;

            return *this;

        }


    private:

};

Point::Point()
{
    id = 0;
    x = 0.0;
    y = 0.0;
    z = 0.0;

}

Point::Point(int id_input,  double x_input,  double y_input,  double z_input)
{
    id = id_input;
    x = x_input;
    y = y_input;
    z = z_input;

}


class Point_along_Segment
{

    public:

        Point_along_Segment(double s, Point pt);

        Point pt;
        double s;

    private:





};


Point_along_Segment::Point_along_Segment(double s_input, Point pt_input)
{

    s = s_input;
    pt = pt_input;

}


bool operator<(Point_along_Segment ps1, Point_along_Segment ps2)
{

    return ps1.s < ps2.s;

}


class Vector
{
    public:

        Vector();
        Vector(Point Point0, Point Point1);
        Point VectorStartPoint, VectorEndPoint;

        double x_comp();
        double y_comp();
        double z_comp();

        double magnitude();
        double magnitude_horiz();
        Vector unitvector();

        Vector vectorbyscalar(Vector vector1,  double scalar1);
        Vector vectdiff(Vector vect2);
        Vector vector_projection(Vector vector1, Vector vector2);

        Point vector2point();

        Vector antivector();
        Vector vectorproduct(Vector inputvector2);
        double scalarproduct(Vector inputvector2);
        double scalarproduct_horiz(Vector inputvector2);
        double vectorsangle(Vector inputvector2);
        double vectorsangle_horiz(Vector inputvector2);

        double vector_dipangle_rad();

        Vector&
        operator=(Vector v1)
        {
            VectorStartPoint = v1.VectorStartPoint;
            VectorEndPoint = v1.VectorEndPoint;

            return *this;

        }


    private:


    protected:


};


double Vector::x_comp()
{
    return VectorEndPoint.x - VectorStartPoint.x;
}


double Vector::y_comp()
{
    return VectorEndPoint.y - VectorStartPoint.y;
}

double Vector::z_comp()
{
    return VectorEndPoint.z - VectorStartPoint.z;
}


Vector operator+(Vector v1, Vector v2)
{

    Vector vector_sum;

    vector_sum.VectorStartPoint =  v1.VectorStartPoint;
    vector_sum.VectorEndPoint = Point( v2.VectorEndPoint.id, v1.VectorEndPoint.x + v2.x_comp(), v1.VectorEndPoint.y + v2.y_comp(), v1.VectorEndPoint.z + v2.z_comp());
    return vector_sum;

}

Vector operator-(Vector v1, Vector v2)
{

    Vector vector_diff;

    Vector antivect_v2 = v2.antivector();

    vector_diff = v1 + antivect_v2;

    return vector_diff;

}


Vector::Vector()
{

VectorStartPoint = Point();
VectorEndPoint = Point();

}


Vector::Vector(Point Point0, Point Point1)
{

VectorStartPoint = Point0;
VectorEndPoint = Point1;

}



double Vector::magnitude() // magnitude of vector
{
 return sqrt(x_comp()*x_comp() + y_comp()*y_comp() + z_comp()*z_comp());
}


double Vector::magnitude_horiz() // magnitude of vector
{


 return sqrt(x_comp()*x_comp() + y_comp()*y_comp());

}


bool operator<(Vector v1, Vector v2)
{

    return v1.magnitude() < v2.magnitude();

}


Vector Vector::unitvector() // normalization of vector
{

 Point outputvector_secondpoint;

 long double magnit = magnitude();

 outputvector_secondpoint.x = ((VectorEndPoint.x - VectorStartPoint.x)/magnit) + VectorStartPoint.x;
 outputvector_secondpoint.y = ((VectorEndPoint.y - VectorStartPoint.y)/magnit) + VectorStartPoint.y;
 outputvector_secondpoint.z = ((VectorEndPoint.z - VectorStartPoint.z)/magnit) + VectorStartPoint.z;


 return Vector(VectorStartPoint, outputvector_secondpoint);
}

Vector Vector::antivector() // vector opposite
{
    Vector antivect;

    antivect.VectorStartPoint = VectorEndPoint;
    antivect.VectorEndPoint = VectorStartPoint;

    return antivect;

}


Point Vector::vector2point() // vector opposite
{
    Point convert_vect;

    convert_vect.x = VectorEndPoint.x - VectorStartPoint.x;
    convert_vect.y = VectorEndPoint.y - VectorStartPoint.y;
    convert_vect.z = VectorEndPoint.z - VectorStartPoint.z;


    return convert_vect;

}



double Vector::scalarproduct(Vector inputvector2) // function scalarproduct
{

Point conv_vect1 = vector2point();
Point conv_vect2 = inputvector2.vector2point();


 //calculations
 return (conv_vect1.x*conv_vect2.x) + (conv_vect1.y*conv_vect2.y) + (conv_vect1.z*conv_vect2.z);


}

double Vector::scalarproduct_horiz(Vector inputvector2) // function scalarproduct
{

Point conv_vect1 = vector2point();
Point conv_vect2 = inputvector2.vector2point();


 //calculations
 return (conv_vect1.x*conv_vect2.x) + (conv_vect1.y*conv_vect2.y);


}


double Vector::vectorsangle(Vector inputvector2) // function vectorsangle output in radians
{

    double magnitude_v1 = magnitude();
    double magnitude_v2 = inputvector2.magnitude();

    double minimum_magnitude = 1.0e-5;

    if (magnitude_v1 < minimum_magnitude || magnitude_v2 < minimum_magnitude) return -1.0;  // undetermined result, due to too small vector(s)

    double cos_vectorsangle = scalarproduct(inputvector2)/(magnitude_v1*magnitude_v2);

    double vectorsangle_rad;

     if (cos_vectorsangle > 1.0) vectorsangle_rad = 0.0;
     else if  (cos_vectorsangle < -1.0) vectorsangle_rad = M_PI;
     else vectorsangle_rad = acos(cos_vectorsangle);

     return vectorsangle_rad;

}


double Vector::vectorsangle_horiz(Vector inputvector2) // function vectorsangle output in radians
{

    double magnitude_horiz_v1 = magnitude_horiz();
    double magnitude_horiz_v2 = inputvector2.magnitude_horiz();

    double minimum_magnitude = 1.0e-5;

    if (magnitude_horiz_v1 < minimum_magnitude || magnitude_horiz_v2 < minimum_magnitude) return -1.0;  // undetermined result, due to too small vector(s)

    double cos_vectorsangle_horiz = scalarproduct_horiz(inputvector2)/(magnitude_horiz_v1*magnitude_horiz_v2);

    double vectorsangle_rad_horiz;

     if (cos_vectorsangle_horiz > 1.0) vectorsangle_rad_horiz = 0.0;
     else if  (cos_vectorsangle_horiz < -1.0) vectorsangle_rad_horiz = M_PI;
     else vectorsangle_rad_horiz = acos(cos_vectorsangle_horiz);

     return vectorsangle_rad_horiz;

}

double Vector::vector_dipangle_rad() // calculates vector dip angle
{

    double mydeltaelevation = VectorEndPoint.z - VectorStartPoint.z;
    double myvector_horizontallength = magnitude_horiz();

    double mydipangle_rad = atan(mydeltaelevation/myvector_horizontallength);

    return mydipangle_rad;
}



int main ()
{

    string separ_string = ",";
    int i,j;

    double my_separation_thresh = 20000; // m , i.e. 20 km


    /* definizione del tipo di elevazione da utilizzare */
    cout << "Enter elevation type (prox, lin): ";
  	string elev_type;
    cin >> elev_type;
    if (elev_type != "prox" && elev_type != "lin")
    {
        cout << "You should type prox or lin\n";
        return 1;

    }



    /* definizione del file di input dei massimi */
    cout << "Enter max input file name: ";
  	string filename_tracciati_max;
    cin >> filename_tracciati_max;

    ifstream infile_tracciati_max;
    infile_tracciati_max.open(filename_tracciati_max.c_str(), ios::binary);

    if (infile_tracciati_max.fail())
    {
    cout << "File di input " << filename_tracciati_max << " non trovato\n";
    return 1;
    }
    else
    {
    cout << "File di input riconosciuto e aperto\n";
    }

    /* definizione del file di input dei minimi */
    cout << "Enter min input file name: ";
  	string filename_tracciati_min;
    cin >> filename_tracciati_min;

    ifstream infile_tracciati_min;
    infile_tracciati_min.open(filename_tracciati_min.c_str(), ios::binary);

    if (infile_tracciati_min.fail())
    {
    cout << "File di input " << filename_tracciati_min << " non trovato\n";
    return 1;
    }
    else
    {
    cout << "File di input riconosciuto e aperto\n";
    }


    // definizione del file di output puntuale
    cout << "Enter output point file name: ";
  	string output_filename;
    cin >> output_filename;

    ofstream outpointfile;
    outpointfile.open(output_filename.c_str(), ios::binary);

    if (outpointfile.fail())
    {
    cout << "File di output " << output_filename << " non creato\n";
    return 1;
    }
    else
    {
    cout << "File di output riconosciuto e aperto\n";
    }


    // definizione del file di output puntuale
    cout << "Enter output line file name - esri: ";
  	string outputline_filename_ai;
    cin >> outputline_filename_ai;

    ofstream outlinefile_ai;
    outlinefile_ai.open(outputline_filename_ai.c_str(), ios::binary);

    if (outlinefile_ai.fail())
    {
    cout << "File di output " << outputline_filename_ai << " non creato\n";
    return 1;
    }
    else
    {
    cout << "File di output riconosciuto e aperto\n";
    }

    // definizione del file di output puntuale
    cout << "Enter output line file name - gml - jump: ";
  	string outputline_filename_jml;
    cin >> outputline_filename_jml;

    ofstream outlinefile_jml;
    outlinefile_jml.open(outputline_filename_jml.c_str(), ios::binary);

    if (outlinefile_jml.fail())
    {
    cout << "File di output " << outputline_filename_jml << " non creato\n";
    return 1;
    }
    else
    {
    cout << "File di output riconosciuto e aperto\n";
    }


    //
    // inizio processamento input dati
    //





    // LETTURA DATI DA FILE INPUT
    string rec_line;
    char comma;



    // LETTURA DATI DA FILE INPUT MAX

    list<input_datum> max_input_data;


    // legge l'header del file
    getline(infile_tracciati_max, rec_line);


    // lettura dei dati di input delle tracce di megadune
    while (!infile_tracciati_max.eof())
    {

        // read record
        getline(infile_tracciati_max, rec_line);

        if (rec_line.size() == 0) continue;

        istringstream instr(rec_line);

        input_datum indat_max;

        instr >> indat_max.id_rec >> comma >> indat_max.id_track >> comma >>
                    indat_max.x >> comma >> indat_max.y >> comma >>
                    indat_max.s >> comma >> indat_max.h >> comma >>
                    indat_max.z_orig >> comma >> indat_max.z_linint  >> comma >>
                    indat_max.z_proxint >> comma >> indat_max.dist_proxelpt;

        /*printf("controllo input max:  %d, %d, %f, %f, %f, %f, %f\n",
                indat_max.id_rec, indat_max.id_track, indat_max.s,
                indat_max.x, indat_max.y, indat_max.z_orig, indat_max.z_int);*/

        max_input_data.push_back(indat_max);


    }



    // LETTURA DATI DA FILE INPUT MIN

    list<input_datum> min_input_data;

    // legge l'header del file
    getline(infile_tracciati_min, rec_line);

    // lettura dei dati di input delle tracce di megadune
    while (!infile_tracciati_min.eof())
    {

        // read record
        getline(infile_tracciati_min, rec_line);

        if (rec_line.size() == 0) continue;

        istringstream instr(rec_line);

        input_datum indat_min;

        instr >> indat_min.id_rec >> comma >> indat_min.id_track >> comma >>
                    indat_min.x >> comma >> indat_min.y >> comma >>
                    indat_min.s >> comma >> indat_min.h >> comma >>
                    indat_min.z_orig >> comma >> indat_min.z_linint  >> comma >>
                    indat_min.z_proxint >> comma >> indat_min.dist_proxelpt;


        min_input_data.push_back(indat_min);


    }


    // chiude file di input tracciati max e min
    infile_tracciati_max.close();
    infile_tracciati_min.close();


    // ---------------------------------

    // processamento dati


    outpointfile << "id_lnk" << separ_string << "id_seg" << separ_string << "type" << separ_string << "p1_id" << separ_string <<
        "p1_x" << separ_string << "p1_y" << separ_string << "p1_z" << separ_string <<
        "p2_x" << separ_string << "p2_y" << separ_string << "p2_z" << separ_string <<
        "horizontal_magnitude" << separ_string << "approximate_amplitude"  << separ_string << "dip_angle" << separ_string <<
        "magnitude_ratio" << separ_string << "dip_angle_ratio" <<
        "\n";


    outlinefile_jml << "<?xml version='1.0' encoding='UTF-8'?>" << "\n" <<
        "<JCSDataFile xmlns:gml='http://www.opengis.net/gml' xmlns:xsi='http://www.w3.org/2000/10/XMLSchema-instance' >" << "\n" <<
        "<JCSGMLInputTemplate>" << "\n" <<
        "<CollectionElement>featureCollection</CollectionElement>" << "\n" <<
        "<FeatureElement>feature</FeatureElement>" << "\n" <<
        "<GeometryElement>geometry</GeometryElement>" << "\n" <<
        "<ColumnDefinitions>" << "\n" <<
        "   <column>" << "\n" <<
        "       <name>ID</name>" << "\n" <<
        "       <type>INTEGER</type>" << "\n" <<
        "       <valueElement elementName='property' attributeName='name' attributeValue='ID'/>" << "\n" <<
        "       <valueLocation position='body'/>" << "\n" <<
        "   </column>" << "\n" <<
        "</ColumnDefinitions>" << "\n" <<
        "</JCSGMLInputTemplate>" << "\n\n" <<
        "<featureCollection>" << "\n";


    list<input_datum>::iterator max_pos;

    // ciclo per ogni punto di massimo

    int curr_track_id = -1;
    Point pt_max_1(0,0,0,0);
    int n=0, m=0;

    for (max_pos=max_input_data.begin(); max_pos!=max_input_data.end(); max_pos++)
    {

        Point pt_max_2((*max_pos).id_rec, (*max_pos).x, (*max_pos).y, (*max_pos).z_proxint);
        if (elev_type == "lin")
        {
            pt_max_2.z = (*max_pos).z_linint;
        }


        if ( (*max_pos).id_track != curr_track_id || pt_max_2.z < 1.0 || pt_max_2.z >= 9999.0) // skip point when first point of individual track or unknown elevation
        {
            curr_track_id = (*max_pos).id_track;
            pt_max_1 = pt_max_2;
            continue;
        }

        Vector vect_max12(pt_max_1,pt_max_2);


        // A - effettua ricerca, per ogni pto quotato delle tracce max, del set di punti entro la distanza massima prefissata ed appartenenti ai minimi
        //      aggiunge i punti minimi candidati in un multiset


        multiset<Vector> vector_multiset;

        list<input_datum>::iterator min_pos;



        for (min_pos=min_input_data.begin(); min_pos!=min_input_data.end(); min_pos++)
        {


            // skip point when unknown elevation
            if (elev_type == "prox" &&
               ((*min_pos).z_proxint < 1.0 || (*min_pos).z_proxint >= 9999.0)) continue;
            if (elev_type == "lin" &&
               ((*min_pos).z_linint < 1.0 || (*min_pos).z_linint >= 9999.0)) continue;


            Point pt_min((*min_pos).id_rec, (*min_pos).x, (*min_pos).y, (*min_pos).z_proxint);
             if (elev_type == "lin")
            {
                pt_min.z = (*min_pos).z_linint;
            }


            Vector vect_max2min(pt_max_2,pt_min);

            if ( vect_max2min.magnitude_horiz() > my_separation_thresh) continue;  // skip if separation larger than maximum allowed value

            vector_multiset.insert(vect_max2min);

        }


        // B - completato A, se la lista non è empty, all'interno del set predetto, individua il pto a minima dist e circa perpendicolare alla traccia locale,
        //        ed il punto antiperpendicolare a minima dist

        bool shortestvector_1_found = false;
        bool shortestvector_2_found = false;
        Vector shortestvector_1, shortestvector_2;




        multiset<Vector>::iterator ms_v_pos;
        for (ms_v_pos = vector_multiset.begin(); ms_v_pos != vector_multiset.end(); ms_v_pos++)
        {

            Vector mymax2minvect = (*ms_v_pos);

            double myanglebetweenvectors_degr = mymax2minvect.vectorsangle_horiz(vect_max12)*180.0/M_PI;

            if ( myanglebetweenvectors_degr > 80.0 && myanglebetweenvectors_degr < 100.0)
            {

                shortestvector_1_found = true; shortestvector_1 = mymax2minvect;
                //cout << mymax2minvect.VectorStartPoint.x << ", " << shortestvector_1.VectorStartPoint.x << "\n";
                break;
            }

        }

        if (!shortestvector_1_found) continue;

        for (ms_v_pos = vector_multiset.begin(); ms_v_pos != vector_multiset.end(); ms_v_pos++)
        {

            Vector mymax2minvect = (*ms_v_pos);

            double myanglebetweenvectors_degr = mymax2minvect.vectorsangle_horiz(vect_max12)*180.0/M_PI;

            if ( myanglebetweenvectors_degr < 80.0 || myanglebetweenvectors_degr > 100.0) continue;

            double myanglebetweenseparationvectors_degr = mymax2minvect.vectorsangle_horiz(shortestvector_1)*180.0/M_PI;

            if ( myanglebetweenseparationvectors_degr > 130.0)
            {
                shortestvector_2_found = true; shortestvector_2 = mymax2minvect;
                //cout << mymax2minvect.VectorStartPoint.x << ", " << shortestvector_2.VectorStartPoint.x << "\n";
                break;
            }

        }

        if (shortestvector_1_found || shortestvector_2_found) n++;

        // C - outputta il risultato in formato lineare e puntuale
        if (shortestvector_1_found)
        {

            m++;

            ostringstream outstr;

            outstr << fixed;

            outstr  << setprecision(0) << setw(13) << m << separ_string
                    << setprecision(0) << setw(13) << n << separ_string
                    << setw(13) << "short" << separ_string
                    << setprecision(0) << setw(13) << shortestvector_1.VectorStartPoint.id << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.x << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.y << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.z << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorEndPoint.x << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorEndPoint.y << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorEndPoint.z << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.magnitude_horiz() << separ_string
                    << setprecision(3) << setw(13) << "-9999.9" << separ_string
                    << setprecision(4) << setw(13) << -(shortestvector_1.vector_dipangle_rad()*180.0/M_PI) << separ_string
                    << setw(13) << "-9999.9" << separ_string
                    << setw(13) << "-9999.9";

            outpointfile << outstr.str() << "\n";

            ostringstream outstr_ln_ai;

            outstr_ln_ai << fixed;

            outstr_ln_ai   << setprecision(0) << setw(13) << m << "\n"
                        << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.x << ","
                        << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.y << ","
                        << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.z << "\n"
                        << setprecision(3) << setw(13) << shortestvector_1.VectorEndPoint.x << ","
                        << setprecision(3) << setw(13) << shortestvector_1.VectorEndPoint.y << ","
                        << setprecision(3) << setw(13) << shortestvector_1.VectorEndPoint.z << "\n"
                        << "END";


            outlinefile_ai << outstr_ln_ai.str() << "\n";


            // scrittura risultati lineari in file in formato gml di jump

            ostringstream outstr_ln_jml;

            outstr_ln_jml << fixed;

            outstr_ln_jml   << "    <feature>" << "\n"
                            << "    <geometry>" << "\n"
                            << "    <gml:LineString>" << "\n"
                            << "    <gml:coordinates>"
                            << setprecision(3) << shortestvector_1.VectorStartPoint.x << ","
                            << setprecision(3) << shortestvector_1.VectorStartPoint.y << "\n"
                            << setprecision(3) << shortestvector_1.VectorEndPoint.x << ","
                            << setprecision(3) << shortestvector_1.VectorEndPoint.y
                            << " </gml:coordinates>" << "\n"
                            << " </gml:LineString>" << "\n"
                            << "    </geometry>" << "\n"
                            << "    <property name='ID'>"
                            << setprecision(0) << m << "</property>" << "\n"
                            << "</feature>" << "\n";

            outlinefile_jml << outstr_ln_jml.str() << "\n";


        }

        if (shortestvector_2_found)
        {

            m++;

            ostringstream outstr;

            outstr << fixed;

            outstr  << setprecision(0) << setw(13) << m << separ_string
                    << setprecision(0) << setw(13) << n << separ_string
                    << setw(13) << "long" << separ_string
                    << setprecision(0) << setw(13) << shortestvector_2.VectorStartPoint.id << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.VectorStartPoint.x << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.VectorStartPoint.y << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.VectorStartPoint.z << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.VectorEndPoint.x << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.VectorEndPoint.y << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.VectorEndPoint.z << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.magnitude_horiz() << separ_string
                    << setprecision(3) << setw(13) << "-9999.9" << separ_string
                    << setprecision(4) << setw(13) << -(shortestvector_2.vector_dipangle_rad()*180.0/M_PI) << separ_string
                    << setw(13) << "-9999.9" << separ_string
                    << setw(13) << "-9999.9";

            outpointfile << outstr.str() << "\n";

            ostringstream outstr_ln_ai;

            outstr_ln_ai << fixed;

            outstr_ln_ai   << setprecision(0) << setw(13) << m << "\n"
                        << setprecision(3) << setw(13) << shortestvector_2.VectorStartPoint.x << ","
                        << setprecision(3) << setw(13) << shortestvector_2.VectorStartPoint.y << ","
                        << setprecision(3) << setw(13) << shortestvector_2.VectorStartPoint.z << "\n"
                        << setprecision(3) << setw(13) << shortestvector_2.VectorEndPoint.x << ","
                        << setprecision(3) << setw(13) << shortestvector_2.VectorEndPoint.y << ","
                        << setprecision(3) << setw(13) << shortestvector_2.VectorEndPoint.z << "\n"
                        << "END";


            outlinefile_ai << outstr_ln_ai.str() << "\n";

            // scrittura risultati lineari in file in formato gml di jump

            ostringstream outstr_ln_jml;

            outstr_ln_jml << fixed;

            outstr_ln_jml   << "    <feature>" << "\n"
                            << "    <geometry>" << "\n"
                            << "    <gml:LineString>" << "\n"
                            << "    <gml:coordinates>"
                            << setprecision(3) << shortestvector_2.VectorStartPoint.x << ","
                            << setprecision(3) << shortestvector_2.VectorStartPoint.y << "\n"
                            << setprecision(3) << shortestvector_2.VectorEndPoint.x << ","
                            << setprecision(3) << shortestvector_2.VectorEndPoint.y
                            << " </gml:coordinates>" << "\n"
                            << " </gml:LineString>" << "\n"
                            << "    </geometry>" << "\n"
                            << "    <property name='ID'>"
                            << setprecision(0) << m << "</property>" << "\n"
                            << "</feature>" << "\n";

            outlinefile_jml << outstr_ln_jml.str() << "\n";


        }

        if (shortestvector_1_found && shortestvector_2_found)
        {

            m++;

            Vector mymegadunevector = shortestvector_1 - shortestvector_2;
            double mymegadune_length = shortestvector_1.magnitude_horiz() + shortestvector_2.magnitude_horiz();
            double mymegadune_minimaverticalstep = shortestvector_2.VectorEndPoint.z - shortestvector_1.VectorEndPoint.z;
            double mymegadune_minimaslope_percent = abs(mymegadune_minimaverticalstep/mymegadune_length);
            double mymegadune_max2min_vect1_vertoffs = shortestvector_1.VectorStartPoint.z - shortestvector_1.VectorEndPoint.z;  // height difference between maximum and minimum for vector 1

            double myinterpolatedheightatmaximumlocation = shortestvector_1.VectorEndPoint.z + (mymegadune_minimaverticalstep*shortestvector_1.magnitude_horiz()/mymegadune_length);
            double mymegadune_approximateamplitude = shortestvector_1.VectorStartPoint.z - myinterpolatedheightatmaximumlocation;


            ostringstream outstr;

            outstr << fixed;

            outstr  << setprecision(0) << setw(13) << m << separ_string
                    << setprecision(0) << setw(13) << n << separ_string
                    << setw(13) << "full" << separ_string
                    << setprecision(0) << setw(13) << shortestvector_1.VectorStartPoint.id << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.x << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.y << separ_string
                    << setprecision(3) << setw(13) << shortestvector_1.VectorStartPoint.z << separ_string
                    << setw(13) << "-9999.9" << separ_string
                    << setw(13) << "-9999.9" << separ_string
                    << setw(13) << "-9999.9" << separ_string
                    << setprecision(3) << setw(13) << mymegadune_length << separ_string
                    << setprecision(3) << setw(13) << mymegadune_approximateamplitude  << separ_string
                    << setprecision(3) << setw(13) << -(mymegadunevector.vector_dipangle_rad()*180.0/M_PI) << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.magnitude_horiz()/shortestvector_1.magnitude_horiz() << separ_string
                    << setprecision(3) << setw(13) << shortestvector_2.vector_dipangle_rad()/shortestvector_1.vector_dipangle_rad();

            outpointfile << outstr.str() << "\n";


        }



        // D - aggiornamento dei valori per il successivo ciclo

        pt_max_1 = pt_max_2;  // aggiorna il valore del massimo 1


    }







    // ---------------------------------

    // conclusione programma



    outpointfile.close();

    outlinefile_ai << "END\n";

    outlinefile_ai.close();


    outlinefile_jml << "</featureCollection>\n</JCSDataFile>";

    outlinefile_jml.close();


    return 0;

}




